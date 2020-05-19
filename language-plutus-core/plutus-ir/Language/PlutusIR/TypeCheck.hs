-- | Kind/type inference/checking.

{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Language.PlutusIR.TypeCheck
    (
    -- * Configuration.
      DynamicBuiltinNameTypes (..)
    , TypeCheckConfig (..)
    , tccDynamicBuiltinNameTypes
    , PLC.defConfig
    , dynamicBuiltinNameMeaningsToTypes
    -- * Kind/type inference/checking.
    , inferKind
    , checkKind
    , typeOfBuiltinName
    , inferType
    , checkType
    , inferTypeOfProgram
    , checkTypeOfProgram
    ) where

import           Language.PlutusCore.Constant
import qualified           Language.PlutusCore.Core as PLC
import Language.PlutusIR
import           Language.PlutusIR.Compiler.Error
import           Language.PlutusIR.Normalize
import           Language.PlutusCore.Quote
import           Language.PlutusCore.Rename
import qualified           Language.PlutusCore.TypeCheck as PLC
import           Language.PlutusIR.TypeCheck.Internal
import           Language.PlutusCore.Universe

import           Control.Monad.Except

{- Note [Goal of PIR typecheker]

Why we do typechecking:

- The deadcode eliminator (at `Optimizer/Deadcode.hs`) may eliminate ill-typed code, which
would turn, much to a surprise, an ill-typed program to a well-typed one.
- The let datatypebinds and/or typebinds introduce new types which may escape from the type of the interm.
Although they may compile fine to PLC and PLC-typecheck correctly, they are invalid in terms of PIR typechecking.
This would disallow such programs. See for example `./test/recursion/even3Eval` and the discussion at
<https://groups.google.com/a/iohk.io/forum/#!msg/plutus/6ycMTngVomc/VKeb00DuHwAJ>
- The error-messages would be more specialized to lets.
- We can have more flexible or more strict let syntax rules for truly recursive/nonrecursive bindings
-}


-- | Extract the 'TypeScheme' from a 'DynamicBuiltinNameMeaning' and convert it to the
-- corresponding @Type TyName@ for each row of a 'DynamicBuiltinNameMeanings'.
dynamicBuiltinNameMeaningsToTypes
    :: (AsTypeError e uni ann, MonadError e m, MonadQuote m, PLC.UniOf term ~ uni)
    => ann -> DynamicBuiltinNameMeanings term -> m (DynamicBuiltinNameTypes uni)
dynamicBuiltinNameMeaningsToTypes ann (DynamicBuiltinNameMeanings means) = do
    let getType mean = do
            let ty = dynamicBuiltinNameMeaningToType mean
            _ <- inferKind PLC.defConfig $ ann <$ ty
            pure <$> normalizeType ty
    DynamicBuiltinNameTypes <$> traverse getType means

-- | Infer the kind of a type.
inferKind
    :: (AsTypeError e uni ann, MonadError e m, MonadQuote m)
    => TypeCheckConfig uni -> Type TyName uni ann -> m (Kind ())
inferKind config = runTypeCheckM config . inferKindM

-- | Check a type against a kind.
-- Infers the kind of the type and checks that it's equal to the given kind
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
checkKind
    :: (AsTypeError e uni ann, MonadError e m, MonadQuote m)
    => TypeCheckConfig uni -> ann -> Type TyName uni ann -> Kind () -> m ()
checkKind config ann ty = runTypeCheckM config . checkKindM ann ty

-- | Infer the type of a term.
inferType
    :: ( AsTypeError e uni ann, MonadError e m, MonadQuote m
       , GShow uni, GEq uni, DefaultUni <: uni
       )
    => TypeCheckConfig uni -> Term TyName Name uni ann -> m (Normalized (Type TyName uni ()))
inferType config = rename >=> runTypeCheckM config . inferTypeM

-- | Check a term against a type.
-- Infers the type of the term and checks that it's equal to the given type
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
checkType
    :: ( AsTypeError e uni ann, MonadError e m, MonadQuote m
       , GShow uni, GEq uni, DefaultUni <: uni
       )
    => TypeCheckConfig uni
    -> ann
    -> Term TyName Name uni ann
    -> Normalized (Type TyName uni ())
    -> m ()
checkType config ann term ty = do
    termRen <- rename term
    runTypeCheckM config $ checkTypeM ann termRen ty

-- | Infer the type of a program.
inferTypeOfProgram
    :: ( AsTypeError e uni ann, MonadError e m, MonadQuote m
       , GShow uni, GEq uni, DefaultUni <: uni
       )
    => TypeCheckConfig uni -> Program TyName Name uni ann -> m (Normalized (Type TyName uni ()))
inferTypeOfProgram config (Program _ term) = inferType config term

-- | Check a program against a type.
-- Infers the type of the program and checks that it's equal to the given type
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
checkTypeOfProgram
    :: (AsTypeError e uni ann, MonadError e m, MonadQuote m
       , GShow uni, GEq uni, DefaultUni <: uni
       )
    => TypeCheckConfig uni
    -> ann
    -> Program TyName Name uni ann
    -> Normalized (Type TyName uni ())
    -> m ()
checkTypeOfProgram config ann (Program _ term) = checkType config ann term
