-- | The internal module of the type checker that defines the actual algorithms,
-- but not the user-facing API.

-- 'makeLenses' produces an unused lens.
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections #-}
module Language.PlutusIR.TypeCheck.Internal
    ( PLC.DynamicBuiltinNameTypes (..)
    , PLC.TypeCheckConfig (..)
    , TypeCheckM
    , PLC.tccDynamicBuiltinNameTypes
    , runTypeCheckM
    , inferKindM
    , checkKindM
    , checkKindOfPatternFunctorM
    , typeOfBuiltinName
    , inferTypeM
    , checkTypeM
    ) where

import           Language.PlutusCore.Constant
import           Language.PlutusIR
import           Language.PlutusIR.Compiler.Error
import           Language.PlutusCore.MkPlc
import           Language.PlutusCore.Name
import           Language.PlutusCore.Normalize
import           Language.PlutusCore.Quote
import           Language.PlutusCore.Rename
import           Language.PlutusCore.Universe
import Language.PlutusIR.Compiler.Datatype
import           PlutusPrelude
import Data.Foldable
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Language.PlutusCore.TypeCheck.Internal as PLC
import qualified Language.PlutusCore.Normalize.Internal as Norm
import qualified Language.PlutusCore.Core as PLC
import qualified Data.Map                               as Map
import qualified Language.PlutusIR.MkPir                as PIR
import Data.List
import qualified Data.Text as T
import Data.Maybe
{- Note [Global uniqueness]
WARNING: type inference/checking works under the assumption that the global uniqueness condition
is satisfied. The invariant is not checked, enforced or automatically fulfilled. So you must ensure
that the global uniqueness condition is satisfied before calling 'inferTypeM' or 'checkTypeM'.

The invariant is preserved. In future we will enforce the invariant.
-}

{- Note [Notation]
We write type rules in the bidirectional style.

[infer| G !- x : a] -- means that the inferred type of 'x' in the context 'G' is 'a'.
'a' is not necessary a varible, e.g. [infer| G !- fun : dom -> cod] is fine too.
It reads as follows: "infer the type of 'fun' in the context 'G', check that it's functional and
bind the 'dom' variable to the domain and the 'cod' variable to the codomain of this type".

Analogously, [infer| G !- t :: k] means that the inferred kind of 't' in the context 'G' is 'k'.
The [infer| G !- x : a] judgement appears in conclusions in the clauses of the 'inferTypeM'
function.

[check| G !- x : a] -- check that the type of 'x' in the context 'G' is 'a'.
Since Plutus Core is a fully elaborated language, this amounts to inferring the type of 'x' and
checking that it's equal to 'a'.

Analogously, [check| G !- t :: k] means "check that the kind of 't' in the context 'G' is 'k'".
The [check| G !- x : a] judgement appears in the conclusion in the sole clause of
the 'checkTypeM' function.

The equality check is denoted as "a ~ b".

We use unified contexts in rules, i.e. a context can carry type variables as well as term variables.

The "NORM a" notation reads as "normalize 'a'".

The "a ~>? b" notations reads as "optionally normalize 'a' to 'b'". The "optionally" part is
due to the fact that we allow non-normalized types during development, but do not allow to submit
them on a chain.

Functions that can fail start with either @infer@ or @check@ prefixes,
functions that cannot fail looks like this:

    kindOfTypeBuiltin
    typeOfConstant
    typeOfBuiltinName
-}

-- ######################
-- ## Type definitions ##
-- ######################

-- | The type checking monad that the type checker runs in.
-- In contains a 'TypeCheckEnv' and allows to throw 'TypeError's.
-- Note: does not reuse the PLC.TypeCheckM because this is specialized to the duplicate PIR.TypeError
type TypeCheckM uni ann = ReaderT (PLC.TypeCheckEnv uni) (ExceptT (TypeError uni ann) Quote)

-- | Run a 'TypeCheckM' computation by supplying a 'TypeCheckConfig' to it.
runTypeCheckM
    :: (AsTypeError e uni ann, MonadError e m, MonadQuote m)
    => PLC.TypeCheckConfig uni -> TypeCheckM uni ann a -> m a
runTypeCheckM config a =
    throwingEither _TypeError =<< liftQuote (runExceptT $ runReaderT a env) where
        env = PLC.TypeCheckEnv config mempty mempty

-- | Extend the context of a 'TypeCheckM' computation with a kinded variable.
withTyVar :: TyName -> Kind () -> TypeCheckM uni ann a -> TypeCheckM uni ann a
withTyVar name = local . over PLC.tceTyVarKinds . insertByName name

-- | Extend the context of a 'TypeCheckM' computation with a typed variable.
withVar :: Name -> Normalized (Type TyName uni ()) -> TypeCheckM uni ann a -> TypeCheckM uni ann a
withVar name = local . over PLC.tceVarTypes . insertByName name . pure

-- | Look up a 'DynamicBuiltinName' in the 'DynBuiltinNameTypes' environment.
lookupDynamicBuiltinNameM
    :: ann -> PLC.DynamicBuiltinName -> TypeCheckM uni ann (Normalized (Type TyName uni ()))
lookupDynamicBuiltinNameM ann name = do
    PLC.DynamicBuiltinNameTypes dbnts <- asks $ PLC._tccDynamicBuiltinNameTypes . PLC._tceTypeCheckConfig
    case Map.lookup name dbnts of
        Nothing ->
            throwError $ UnknownDynamicBuiltinName ann (UnknownDynamicBuiltinNameErrorE name)
        Just ty -> liftDupable ty

-- | Look up a type variable in the current context.
lookupTyVarM :: ann -> TyName -> TypeCheckM uni ann (Kind ())
lookupTyVarM ann name = do
    mayKind <- asks $ lookupName name . PLC._tceTyVarKinds
    case mayKind of
        Nothing   -> throwError $ FreeTypeVariableE ann name
        Just kind -> pure kind

-- | Look up a term variable in the current context.
lookupVarM :: ann -> Name -> TypeCheckM uni ann (Normalized (Type TyName uni ()))
lookupVarM ann name = do
    mayTy <- asks $ lookupName name . PLC._tceVarTypes
    case mayTy of
        Nothing -> throwError $ FreeVariableE ann name
        Just ty -> liftDupable ty

-- #############
-- ## Dummies ##
-- #############

dummyUnique :: Unique
dummyUnique = Unique 0

dummyTyName :: TyName
dummyTyName = TyName (Name "*" dummyUnique)

dummyKind :: Kind ()
dummyKind = Type ()

dummyType :: Type TyName uni ()
dummyType = TyVar () dummyTyName

dummyTerm :: Term tyname Name unia ()
dummyTerm = Var () (Name "__var" dummyUnique)

-- ########################
-- ## Type normalization ##
-- ########################

-- | Normalize a 'Type'.
normalizeTypeM :: Type TyName uni ann1 -> TypeCheckM uni ann (Normalized (Type TyName uni ann1))
normalizeTypeM ty = Norm.runNormalizeTypeM $ Norm.normalizeTypeM ty

-- | Substitute a type for a variable in a type and normalize the result.
substNormalizeTypeM
    :: Normalized (Type TyName uni ())  -- ^ @ty@
    -> TyName                           -- ^ @name@
    -> Type TyName uni ()               -- ^ @body@
    -> TypeCheckM uni ann (Normalized (Type TyName uni ()))
substNormalizeTypeM ty name body = Norm.runNormalizeTypeM $ Norm.substNormalizeTypeM ty name body

-- ###################
-- ## Kind checking ##
-- ###################

-- | Infer the kind of a type.
inferKindM :: Type TyName uni ann -> TypeCheckM uni ann (Kind ())

-- b :: k
-- ------------------------
-- [infer| G !- con b :: k]
inferKindM (TyBuiltin _ _)         =
    pure $ Type ()

-- [infer| G !- v :: k]
-- ------------------------
-- [infer| G !- var v :: k]
inferKindM (TyVar ann v)           =
    lookupTyVarM ann v

-- [infer| G , n :: dom !- body :: cod]
-- -------------------------------------------------
-- [infer| G !- (\(n :: dom) -> body) :: dom -> cod]
inferKindM (TyLam _ n dom body)    = do
    let dom_ = void dom
    withTyVar n dom_ $ KindArrow () dom_ <$> inferKindM body

-- [infer| G !- fun :: dom -> cod]    [check| G !- arg :: dom]
-- -----------------------------------------------------------
-- [infer| G !- fun arg :: cod]
inferKindM (TyApp ann fun arg)     = do
    funKind <- inferKindM fun
    case funKind of
        KindArrow _ dom cod -> do
            checkKindM ann arg dom
            pure cod
        _ -> throwError $ KindMismatch ann (void fun) (KindArrow () dummyKind dummyKind) funKind

-- [check| G !- a :: *]    [check| G !- b :: *]
-- --------------------------------------------
-- [infer| G !- a -> b :: *]
inferKindM (TyFun ann dom cod)     = do
    checkKindM ann dom $ Type ()
    checkKindM ann cod $ Type ()
    pure $ Type ()

-- [check| G , n :: k !- body :: *]
-- ---------------------------------------
-- [infer| G !- (all (n :: k). body) :: *]
inferKindM (TyForall ann n k body) = do
    withTyVar n (void k) $ checkKindM ann body (Type ())
    pure $ Type ()

-- [infer| G !- arg :: k]    [check| G !- pat :: (k -> *) -> k -> *]
-- -----------------------------------------------------------------
-- [infer| G !- ifix pat arg :: *]
inferKindM (TyIFix ann pat arg)    = do
    k <- inferKindM arg
    checkKindOfPatternFunctorM ann pat k
    pure $ Type ()

-- | Check a 'Type' against a 'Kind'.
checkKindM :: ann -> Type TyName uni ann -> Kind () -> TypeCheckM uni ann ()

-- [infer| G !- ty : tyK]    tyK ~ k
-- ---------------------------------
-- [check| G !- ty : k]
checkKindM ann ty k = do
    tyK <- inferKindM ty
    when (tyK /= k) $ throwError (KindMismatch ann (void ty) k tyK)

-- | Check that the kind of a pattern functor is @(k -> *) -> k -> *@.
checkKindOfPatternFunctorM
    :: ann
    -> Type TyName uni ann  -- ^ A pattern functor.
    -> Kind ()              -- ^ @k@.
    -> TypeCheckM uni ann ()
checkKindOfPatternFunctorM ann pat k =
    checkKindM ann pat $ KindArrow () (KindArrow () k (Type ())) (KindArrow () k (Type ()))

-- ###################
-- ## Type checking ##
-- ###################

-- | Return the 'Type' of a 'BuiltinName'.
typeOfBuiltinName
    :: (GShow uni, GEq uni, DefaultUni <: uni)
    => PLC.BuiltinName -> Type TyName uni ()
typeOfBuiltinName bn = withTypedBuiltinName bn $ typeOfTypedBuiltinName @(PLC.Term TyName Name _ ())

-- | @unfoldFixOf pat arg k = NORM (vPat (\(a :: k) -> ifix vPat a) arg)@
unfoldFixOf
    :: Normalized (Type TyName uni ())  -- ^ @vPat@
    -> Normalized (Type TyName uni ())  -- ^ @vArg@
    -> Kind ()                      -- ^ @k@
    -> TypeCheckM uni ann (Normalized (Type TyName uni ()))
unfoldFixOf pat arg k = do
    let vPat = unNormalized pat
        vArg = unNormalized arg
    a <- liftQuote $ freshTyName "a"
    normalizeTypeM $
        mkIterTyApp () vPat
            [ TyLam () a k . TyIFix () vPat $ TyVar () a
            , vArg
            ]

-- | Infer the type of a 'Builtin'.
inferTypeOfBuiltinM
    :: (GShow uni, GEq uni, DefaultUni <: uni)
    => PLC.Builtin ann -> TypeCheckM uni ann (Normalized (Type TyName uni ()))
-- We have a weird corner case here: the type of a 'BuiltinName' can contain 'TypedBuiltinDyn', i.e.
-- a static built-in name is allowed to depend on a dynamic built-in type which are not required
-- to be normalized. For dynamic built-in names we store a map from them to their *normalized types*,
-- with the normalization happening in this module, but what should we do for static built-in names?
-- Right now we just renormalize the type of a static built-in name each time we encounter that name.
inferTypeOfBuiltinM (PLC.BuiltinName    _   name) = normalizeType $ typeOfBuiltinName name
-- TODO: inline this definition once we have only dynamic built-in names.
inferTypeOfBuiltinM (PLC.DynBuiltinName ann name) = lookupDynamicBuiltinNameM ann name

-- See the [Global uniqueness] and [Type rules] notes.
-- | Synthesize the type of a term, returning a normalized type.
inferTypeM
    :: forall uni ann. (GShow uni, GEq uni, DefaultUni <: uni)
    => Term TyName Name uni ann -> TypeCheckM uni ann (Normalized (Type TyName uni ()))

-- c : vTy
-- -------------------------
-- [infer| G !- con c : vTy]
inferTypeM (Constant _ (Some (ValueOf uni _))) =
    -- See Note [PLC types and universes].
    pure . Normalized . TyBuiltin () $ Some (TypeIn uni)

-- [infer| G !- bi : vTy]
-- ------------------------------
-- [infer| G !- builtin bi : vTy]
inferTypeM (Builtin _ bi)           =
    inferTypeOfBuiltinM bi

-- [infer| G !- v : ty]    ty ~>? vTy
-- ----------------------------------
-- [infer| G !- var v : vTy]
inferTypeM (Var ann name)           =
    lookupVarM ann name

-- [check| G !- dom :: *]    dom ~>? vDom    [infer| G , n : dom !- body : vCod]
-- -----------------------------------------------------------------------------
-- [infer| G !- lam n dom body : vDom -> vCod]
inferTypeM (LamAbs ann n dom body)  = do
    checkKindM ann dom $ Type ()
    vDom <- normalizeTypeM $ void dom
    TyFun () <<$>> pure vDom <<*>> withVar n vDom (inferTypeM body)

-- [infer| G , n :: nK !- body : vBodyTy]
-- ---------------------------------------------------
-- [infer| G !- abs n nK body : all (n :: nK) vBodyTy]
inferTypeM (TyAbs _ n nK body)      = do
    let nK_ = void nK
    TyForall () n nK_ <<$>> withTyVar n nK_ (inferTypeM body)

-- [infer| G !- fun : vDom -> vCod]    [check| G !- arg : vDom]
-- ------------------------------------------------------------
-- [infer| G !- fun arg : vCod]
inferTypeM (Apply ann fun arg)      = do
    vFunTy <- inferTypeM fun
    case unNormalized vFunTy of
        TyFun _ vDom vCod -> do
            -- Subparts of a normalized type, so normalized.
            checkTypeM ann arg $ Normalized vDom
            pure $ Normalized vCod
        _ -> throwError (TypeMismatch ann (void fun) (TyFun () dummyType dummyType) vFunTy)

-- [infer| G !- body : all (n :: nK) vCod]    [check| G !- ty :: tyK]    ty ~>? vTy
-- --------------------------------------------------------------------------------
-- [infer| G !- body {ty} : NORM ([vTy / n] vCod)]
inferTypeM (TyInst ann body ty)     = do
    vBodyTy <- inferTypeM body
    case unNormalized vBodyTy of
        TyForall _ n nK vCod -> do
            checkKindM ann ty nK
            vTy <- normalizeTypeM $ void ty
            substNormalizeTypeM vTy n vCod
        _ -> throwError (TypeMismatch ann (void body) (TyForall () dummyTyName dummyKind dummyType) vBodyTy)

-- [infer| G !- arg :: k]    [check| G !- pat :: (k -> *) -> k -> *]    pat ~>? vPat    arg ~>? vArg
-- [check| G !- term : NORM (vPat (\(a :: k) -> ifix vPat a) vArg)]
-- -------------------------------------------------------------------------------------------------
-- [infer| G !- iwrap pat arg term : ifix vPat vArg]
inferTypeM (IWrap ann pat arg term) = do
    k <- inferKindM arg
    checkKindOfPatternFunctorM ann pat k
    vPat <- normalizeTypeM $ void pat
    vArg <- normalizeTypeM $ void arg
    checkTypeM ann term =<< unfoldFixOf vPat vArg k
    pure $ TyIFix () <$> vPat <*> vArg

-- [infer| G !- term : ifix vPat vArg]    [infer| G !- vArg :: k]
-- -----------------------------------------------------------------------
-- [infer| G !- unwrap term : NORM (vPat (\(a :: k) -> ifix vPat a) vArg)]
inferTypeM (Unwrap ann term)        = do
    vTermTy <- inferTypeM term
    case unNormalized vTermTy of
        TyIFix _ vPat vArg -> do
            k <- inferKindM $ ann <$ vArg
            -- Subparts of a normalized type, so normalized.
            unfoldFixOf (Normalized vPat) (Normalized vArg) k
        _                  -> throwError (TypeMismatch ann (void term) (TyIFix () dummyType dummyType) vTermTy)
-- [check| G !- ty :: *]    ty ~>? vTy
-- -----------------------------------
-- [infer| G !- error ty : vTy]
inferTypeM (Error ann ty)           = do
    checkKindM ann ty $ Type ()
    normalizeTypeM $ void ty

inferTypeM (Let ann1 recurs bs inTerm) = do
    tyInTerm <- case recurs of
        -- [check| G !- ty :: *]  [infer| G,withBindsLinear(b) !- inT : ty] (forall b in bs. [checkWellFormed| Glinear !- b]) 
        -- -------------------------------------------------
        -- [infer| G !- (let (nonrec) bs in inT) :: ty
        NonRec ->
            foldr
               (\b acc -> do
                   checkWellformBind NonRec b
                   -- linear scope: the new identifiers will be visible in the next binding
                   withBind b acc)
               (inferTypeM inTerm)
               bs
        -- [check| G !- ty :: *]  G'=G,withBinds(bs)  [infer| G' !- inT : ty] (forall b in bs. [checkWellFormed| G' !- b])
        -- -------------------------------------------------
        -- [infer| G !- (let (rec) bs in inT) :: ty
        Rec -> do
            -- Check that there are no conflicting new identifiers (same-name)
            -- that are introduced by the bindings of this the letrec
            -- separately check the VarTypes environment for conflicts
            -- TODO: move these 2 checks to a different pass, e.g. in renaming phase
            assertNoRecConflict $ foldMap bindingNewIds bs
            -- separately check the VarkInds environment for conflicts
            assertNoRecConflict . mapMaybe bindingNewTyId $ toList bs
            -- now with all bindings in the env, check for wellformedness
            withBinds bs $ do
              checkWellformBinds bs
              inferTypeM inTerm
    -- In paper's Fig.6, T-Let and T-LetRec rules dictate that: G !- inTerm :: *
    -- Durining implemenetation, however, this leads to scoping errors because
    -- the G env is not augmented to see the kinds introduced by any (nested) let(s)
    -- So we disable this check: `checkKindM ann (ann <$ unNormalized tyInTerm) $ Type ()`
    -- Furthermore, this check would be unnecessary afterall, because of the proof-by-construction:
    -- All terms that can be constructed have types that are *-kinded.
    pure tyInTerm

  where
    withBinds :: NonEmpty (Binding TyName Name uni ann) -> TypeCheckM uni ann a -> TypeCheckM uni ann a
    withBinds = flip . foldr $ withBind

    checkWellformBinds :: NonEmpty (Binding TyName Name uni ann) -> TypeCheckM uni ann ()
    checkWellformBinds = traverse_ $ checkWellformBind Rec

    assertNoRecConflict :: [T.Text] -> TypeCheckM uni ann ()
    assertNoRecConflict = maybe (pure ())
                          (throwError . ConflictingRecBinds ann1)
                           . hasDuplicate

    -- TODO: duplicate code, also move to PlutusIR.hs
    bindingNewIds :: Binding TyName Name uni ann -> [T.Text]
    bindingNewIds = \case
        TermBind _ _ d _ -> [nameString $ varDeclName d]
        DatatypeBind _ (Datatype _ _ _ des vdecls) ->
            nameString des : fmap (nameString . varDeclName) vdecls
        _typebind -> []

    -- a binding may introduce max. 1 new id in scope, hence maybe
    -- TODO: duplicate code, also move to PlutusIR.hs
    bindingNewTyId :: Binding TyName Name uni ann -> Maybe T.Text
    bindingNewTyId = \case
        TypeBind _ tvdecl _ ->
            pure . nameString . unTyName $ tyVarDeclName tvdecl
        DatatypeBind _ (Datatype _ tvdecl _ _ _) ->
            pure . nameString . unTyName $ tyVarDeclName tvdecl
        _termbind -> mempty

-- See the [Global uniqueness] and [Type rules] notes.
-- | Check a 'Term' against a 'NormalizedType'.
checkTypeM
    :: (GShow uni, GEq uni, DefaultUni <: uni)
    => ann -> Term TyName Name uni ann -> Normalized (Type TyName uni ()) -> TypeCheckM uni ann ()

-- [infer| G !- term : vTermTy]    vTermTy ~ vTy
-- ---------------------------------------------
-- [check| G !- term : vTy]
checkTypeM ann term vTy = do
    vTermTy <- inferTypeM term
    when (vTermTy /= vTy) $ throwError (TypeMismatch ann (void term) (unNormalized vTermTy) vTy)

-- | Extends the environment(s) with a bind.
-- NOTE: there are no well-formed or duplication checks here, it just adds to the environment(s) like withVar/withTyVar do.
-- For checks see `checkWellformBind`.
withBind :: forall uni ann a.
           Binding TyName Name uni ann
         -> TypeCheckM uni ann a
         -> TypeCheckM uni ann a
withBind b m = case b of
  TermBind _ _ (VarDecl _ n ty) _rhs -> do
      -- OPTIMIZE: redundant, usually this function is called together with `checkWellformBind`,
      -- which performs normalization there as well
      vTy <- normalizeTypeM $ void ty
      withVar n vTy m
  TypeBind _ (TyVarDecl _ tn k) _rhs ->
      withTyVar tn (void k) m
  DatatypeBind _ dt@(Datatype ann (TyVarDecl _ tn k) tyargs des vdecls) -> do
      desType <- mkDestructorType
      -- normalize all data-constructors's types.
      normConstrs <- for vdecls $ \(VarDecl _ n ty) ->
                                     -- make sure to add the implicitly-qualified tyargs foralls for each constructor.
                                     (n,) <$> (normalizeTypeM $ void $ PIR.mkIterTyForall tyargs ty)
      -- extend the type-environment with data-destructor+data-constructors
      withVars ((des,desType):normConstrs) $
          -- extend the kind-environment with type-constructor
          withTyVar tn (void k) m
   where
       mkDestructorType :: TypeCheckM uni ann (Normalized (Type TyName uni ()))
       mkDestructorType = do
           -- get a fresh result type
           out <- resultTypeName dt
           normalizeTypeM $ void $
             -- forall (a1::*) (a2::*) ... .
             PIR.mkIterTyForall tyargs $
               TyFun ann
                     -- first argument is datatype:  T a1 a2 ... ->
                     (PIR.mkIterTyApp ann (TyVar ann tn) (fmap (mkTyVar ann) tyargs ))
                     -- forall (out :: *)
                     (TyForall ann out (Type ann) $
                       -- the constructors' types with result-type replaced by out
                       -- note to self: no need to normalize the individual constructor here;
                       -- the outer,final,surrounding normalizeTypeM will reach and normalize each constructor
                       foldr
                       (TyFun ann . constructorCaseType (TyVar ann out))
                       -- result type of constructor is: out
                       (TyVar ann out)
                       vdecls
                     )

       withVars :: Foldable t
                => t (Name, Normalized (Type TyName uni ()))
                -> TypeCheckM uni ann a -> TypeCheckM uni ann a
       withVars names m1 = foldr (uncurry withVar) m1 names

-- | check that a binding is well-formed (correctly typed, kinded)
checkWellformBind :: forall uni ann.
                     (GShow uni, GEq uni, DefaultUni <: uni)
                    => Recursivity
                    -> Binding TyName Name uni ann
                    -> TypeCheckM uni ann ()
checkWellformBind recurs = \case
  -- W-Term
  TermBind _ _ (VarDecl ann _ ty) rhs -> do
      checkKindM ann ty $ Type ()
      -- OPTIMIZE: redundant, usually this function is called together with `withBind`,
      -- which performs normalization here as well
      vTy <- normalizeTypeM $ void ty
      checkTypeM ann rhs vTy
  -- W-Type
  TypeBind _ (TyVarDecl ann _ k) rhs ->
      checkKindM ann rhs (void k)
  -- W-Data + W-Con
  DatatypeBind _ (Datatype annD tvdecl@(TyVarDecl _ dataName _) tyargs des vdecls) -> do
      -- Checks that tycons+tyargs are unique among eachother, and dataconstructors are unique among eachother
      -- TODO: move these 2 checks to a different pass, e.g. in renaming phase
      assertNoDuplicate allNewTyNames
      assertNoDuplicate allNewNames
      -- check the data-constructors' to be kinded by *
      for_ vdecls $ \(VarDecl annV _ ty) -> do
                  -- we normalize the type, since the user might have written some type-computation in the dataconstructor.
                  -- we split the normalized type to a bunch of constructors arguments and a result-type.
                  Normalized normTy <- normalizeTypeM ty
                  let actualArgsTypes = funTyArgs normTy
                      actualResType = funResultType normTy
                  -- Check that the result-type is *-kinded, inside the whole env scope.
                  -- The result-type can see all tyvardecls (tyargs + currently-defined type constructor).
                  withTyVarDecls allNewTyDecls $
                      checkKindM annV actualResType $ Type ()
                  -- Check the dataconstructor arguments' types to also be *-kinded,
                  -- but the environment defers for rec and nonrec bindings.
                  withTyVarDecls
                      (case recurs of
                           -- With the whole env  in scope.
                           Rec -> allNewTyDecls
                           -- The type-constructor is *not visible* in the arguments' types.
                           NonRec -> tyargs
                      ) $ -- check each argument in the defined environment.
                          for_ actualArgsTypes $ \actualArgType ->
                             checkKindM annV actualArgType $ Type ()
                  -- Check for wellformedness of the result-type to be in the expected form (see `expectedDataConsResType`)
                  -- Note1: in the paper this check for the result-type welformnedness is not needed,
                  -- because the result-type is "implicit" (and not explicit in the FIR syntax).
                  when (expectedDataConsResType /= void actualResType) $
                      throwError $ MalformedDataConstrResType annV expectedDataConsResType
   where
      allNewTyDecls :: [TyVarDecl TyName ann]
      allNewTyDecls = tvdecl:tyargs

      allNewTyNames :: [T.Text]
      allNewTyNames = fmap (nameString . unTyName . tyVarDeclName) allNewTyDecls

      allNewNames :: [T.Text]
      allNewNames = nameString des : fmap (nameString . varDeclName) vdecls

      assertNoDuplicate :: [T.Text] -> TypeCheckM uni ann ()
      assertNoDuplicate = maybe (pure ())
                                (throwError . DuplicateDeclaredIdent annD)
                          . hasDuplicate

      withTyVarDecls :: [TyVarDecl TyName ann] -> TypeCheckM uni ann () -> TypeCheckM uni ann ()
      withTyVarDecls tyvardecls m1 = foldr (\(TyVarDecl _ n k) -> withTyVar n (void k)) m1 tyvardecls

      -- The expected dataconstructor's result-type is of the form `[TyCon tyarg1 tyarg2 ... tyargn]`
      -- Note2: if GADTs are later added to the language, the expected constructor result type has to be relaxed,
      -- i.e. we should only check that`DT` is applied to the correct *number* of tyargs as the number of tvdecls.
      expectedDataConsResType :: Type TyName uni ()
      expectedDataConsResType = foldl
          (\acc (TyVarDecl _ tyarg _) -> TyApp () acc $ TyVar () tyarg)
          (TyVar () dataName)
          tyargs


-- HELPERS

hasDuplicate :: Ord a => [a] -> Maybe a
hasDuplicate l = head <$> (find (\g -> length g > 1) .  group $ sort l)

