{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Language.PlutusIR.Transform.LetFloat (floatTerm) where

import           Language.PlutusIR
import           Language.PlutusIR.Analysis.Dependencies
import           Language.PlutusIR.Analysis.Free

import           Control.Lens
import           Control.Monad.RWS
import           Control.Monad.State

import qualified Language.PlutusCore                     as PLC
import qualified Language.PlutusCore.Name                as PLC

import qualified Algebra.Graph.AdjacencyMap              as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm    as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap     as AMN

import           Data.Foldable                           (fold)
import qualified Data.IntMap                             as IM
import qualified Data.Map                                as M
import           Data.Maybe                              (fromMaybe)
import qualified Data.Set                                as S
import Data.Function (on)

-- | For each Let-binding we compute its minimum "rank", which refers to a dependant lambda/Lambda location that this Let-rhs can topmost/highest float upwards to (w/o having out-of-scope errors)
-- In other words, this is a pointer to a lambda location in the PIR program.
data Rank = LamDep { _depth :: Int, _name :: PLC.Unique }
          deriving Eq
makeLenses ''Rank

-- TODO: Getter for Top
--

instance Ord Rank where
  compare = compare `on` _depth

-- acts as Data.Semigroup.Max
instance Semigroup Rank where
  r1 <> r2 = max r1 r2

instance Monoid Rank where
  mempty = LamDep { _depth = 0                              -- depth starts counting from 0
                  , _name = PLC.Unique { PLC.unUnique = -1} -- dummy name
                  }

-- | During the first pass of the AST, a reader context holds the current in-scope stack of lambdas, as "potential ranks" for consideration.
type Ctx = [Rank] -- the enclosing lambdas in scope that surround a let

-- | During the first pass of the AST, we accumulate into a state the minimum rank of each lets encountered.
type RankData = M.Map
                 PLC.Unique -- ^ the identifier introduced by a let-binding
                 ( Rank-- ^ the minimum rank for this let binding, i.e. where this identifier can topmost/highest float upwards to
                 , PLC.Unique   -- ^ a principal name for this binding, used only because a let-Datatype-bind can introduce multiple identifiers
                 )

-- | During the second pass of the AST, we transform the 'RankData' to an assoc. list of depth=>lambda=>{let_unique} mappings left to process. This assoc. list is sorted by depth for easier code generation.
type DepthData = IM.IntMap         -- ^ the depth
                    (M.Map       -- ^ a mapping of lambdanames belonging to this depth => letidentifiers to float
                      PLC.Unique    -- ^ a lambda name
                      (S.Set PLC.Unique)  -- ^ the let bindings that should be floated/placed under this lambda
                    )

-- | A simple table holding a let-introduced identifier to its RHS.
--
-- Let-groups do not exist in this representation.
-- The Recursivity&Annotation of a prior let-group are copied to each of its RHS entry in the table.
type RhsTable tyname name a = M.Map
                              PLC.Unique
                              (Rhs tyname name a)

-- | An Rhs is a triple of Annotation, Recursivity, and its Binding.
--
-- If the RHS was prior belonging to a let-group, its Recursivity&Annotation refers to (is copied from) the let-group's Recursivity&Annotation.
type Rhs tyname name ann = (ann, Recursivity, Binding tyname name ann)

-- | This function takes a 'Term', cleans the 'Term' from all its 'Let'-bindings and stores those lets into a separate table.
-- Note: the output term is most-likely not a valid PIR program anymore.
removeLets :: forall name tyname a
            . (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
           => Term tyname name a
           -> (Term tyname name a, RhsTable tyname name a)
removeLets = flip runState M.empty . go
 where
   go :: Term tyname name a -> State (RhsTable tyname name a) (Term tyname name a)
   go = \case
         -- this overrides the 'termSubterms' functionality only for the 'Let' constructor
         Let a r bs tIn -> do
           forM_ bs $ \ b -> do
                               b' <- bindingSubterms go b
                               modify (M.insert (bindingUnique b') (a, r, b'))
           go tIn
         t -> termSubterms go t

-- | Traverses a Term to create a mapping of every let variable inside the term ==> to its corresponding rank.
pass1Term ::  forall name tyname a
           . (Ord (tyname a), PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
          => Term tyname name a -> RankData
pass1Term pir = fst $ execRWS (pass1Term' pir) [] M.empty
 where

  -- TODO: rename goTerm, etc

  pass1Term' :: Term tyname name a
             -> RWS Ctx () RankData ()
  pass1Term' = \case
    -- overrides termSubterms for small&big Lambda, and Let (nonrec,rec)

    LamAbs _ n _ tBody  -> pass1Body n tBody
    TyAbs _ n _ tBody   -> pass1Body n tBody

    Let _ NonRec bs tIn -> pass1Let NonRec bs tIn
    Let _ Rec bs tIn    -> pass1Let Rec bs tIn

    x                   -> forM_ (x ^.. termSubterms) pass1Term'

  pass1Body :: PLC.HasUnique b _c => b -> Term tyname name a -> RWS Ctx () RankData ()
  pass1Body n tBody =
    -- adds lambda/Lambda to reader-scope
    local (\ ctx ->  LamDep { _depth = (case ctx of
                                         []  -> mempty
                                         h:_ -> h) ^.depth+1   -- increase depth by 1 compare to outer scope
                            , _name = n^.PLC.unique.coerced
                            } : ctx
          )
    $ pass1Term' tBody

  pass1Let :: Recursivity
          -> [Binding tyname name a]
          -> Term tyname name a
          -> RWS Ctx () RankData ()

  -- TODO: change it to remove State and return Data

  pass1Let recurs bs tIn = do
    -- visit all binding subterms
    forM_ (bs^..traverse.bindingSubterms) pass1Term'

    case recurs of
      -- in nonrec, every rhs has its own FreeSet
      NonRec -> forM_ bs $ \b ->
                             let freeVars = fBinding b -- this means that we see each binding individually, not at the whole let level
                                 princ = bindingUnique b
                                 us = bindingIds b
                             in recordRanks freeVars princ us

      -- in rec, all rhs'es share the same FreeSet
      Rec -> let freeVars = foldMap fBinding bs S.\\ foldMap bindingIds bs
             in forM_ bs $ \ b ->
                             let princ = bindingUnique b
                                 us = bindingIds b
                             in recordRanks freeVars princ us
    -- finally, visit the in-term
    pass1Term' tIn

  -- helper to record a rank for each of the identifiers introduced  by the binding
  recordRanks :: S.Set PLC.Unique -> PLC.Unique -> S.Set PLC.Unique -> RWS Ctx () RankData ()
  recordRanks freeVars princ us = do
    lamRanks <- M.fromList . map (\ l -> (l^.name, l)) <$> ask
    modify $ \letRanks ->
     let freeRanks = M.restrictKeys (fmap fst letRanks <> lamRanks) freeVars
         -- Take the maximum rank of all free vars as the current rank of the let, or TopRank if no free var deps
         maxRank = fold freeRanks
     in foldr (\ i -> M.insert i (maxRank, princ)) letRanks us


floatTerm :: forall name tyname a.
             (Ord (tyname a), PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique, Monoid a)
             => Term tyname name a -> Term tyname name a
floatTerm pir = floatBody mempty depthData pirClean
 where
  depthData = IM.toAscList $ toDepthData $ pass1Term pir -- run the first pass
  -- Visiting a term to apply the float transformation
  visitTerm curRank remInfo = \case
      LamAbs a n ty tBody -> LamAbs a n ty (floatAbs n tBody)
      TyAbs a n k tBody  -> TyAbs a n k (floatAbs n tBody)
      t -> over termSubterms (visitTerm curRank remInfo) t -- descend
    where
      floatAbs :: PLC.HasUnique b _c => b -> Term tyname name a -> Term tyname name a
      floatAbs n tBody = floatBody (depth+~1 $ name.~(n ^. PLC.unique . coerced) $ curRank) remInfo tBody


  -- Special case of 'visitTerm' where we visit a lambda or a Lambda body
  -- TODO: maybe change this to IntMap.foldl
  floatBody _  [] tBody = tBody -- no lets are left to float, do not descend
  floatBody curRank depthTable@((searchingForDepth, lams_lets):restDepthTable) tBody
    | curRank^.depth < searchingForDepth = visitTerm curRank depthTable tBody
    | curRank^.depth == searchingForDepth =
        let tBody' = visitTerm curRank restDepthTable tBody
        in case M.lookup (curRank^.name) lams_lets of
             Just lets -> wrapLets lets curRank restDepthTable tBody'
             _         -> tBody'
    | otherwise = error "just for completion"

  -- TODO: we can transform easily the above visits to a Reader CurDepth
  -- TODO: using the above pure/local way has the disadvantage that we visit a bit more than we need. To fix this we can use State DepthDataRemaining instead.


  (pirClean :: Term tyname name a,
   letTable :: RhsTable tyname name a) = removeLets pir

  -- the dependency graph (as the one used by the deadcode elimination)
  -- but w/ot root node and only uses the name of the var (PLC.Unique) as the node id
  depGraph :: AM.AdjacencyMap PLC.Unique
  depGraph = AM.gmap (\case Variable u -> u; _ -> error "just for completion")
             $ AM.removeVertex Root -- we remove Root because we do not care about it
             $ runTermDeps pir

  -- the dependency graph as before, but with datatype-related nodes merged/grouped under a single node per datatypebind a, see 'bindingUnique'.
  reducedDepGraph :: AM.AdjacencyMap PLC.Unique
  reducedDepGraph = M.foldr (\ (_,_,b) accGraph ->
                              case b of
                                DatatypeBind _ dt -> let princ = bindingUnique b
                                                     in foldr (\ assocB -> AM.replaceVertex assocB princ) accGraph $ datatypeIds dt
                                _ -> accGraph) depGraph letTable

  -- take the strongly-connected components of the reduced dep graph, because it may contain loops (introduced by the LetRecs)
  -- topologically sort these sccs, since we rely on linear (sorted) scoping in our 'wrapLets' code generation
  topSortedSccs :: [AMN.AdjacencyMap PLC.Unique]
  topSortedSccs = fromMaybe (error "Cycle detected in the depgraph. This shouldn't happen in the first place.") $ AM.topSort $ AM.scc reducedDepGraph

  -- | Tries to wrap a given term with newly-generated Let expression(s), essentially floating some Let-Rhses.
  -- The given set of lets is not sorted w.r.t. linear scoping, so this function uses the 'topSortedSccs' of the dependency graph,
  -- to figure out the order in which to generate those new Lets.
  --
  -- The resulting term is wrapped with linear-scope-sorted LetRecs and LetNonRecs (interspersed between each other because letnonrec and letrec syntax cannot be combined)
  -- Example: `let {i = e, ...} in let rec {j = e, ...} in let rec {...} in let {...} in ..... in originalTerm`
  wrapLets :: S.Set PLC.Unique -- ^ the lets to wrap around a term
           -> Rank -- ^ current rank
           -> [(Int, M.Map PLC.Unique (S.Set PLC.Unique))] -- ^ the table remaining
           -> Term tyname name a                           -- ^ the term to be wrapped
           -> Term tyname name a                           -- ^ the final wrapped term
  wrapLets lets curRank restDepthTable t =
    foldl (\ acc dcc ->
             let vs = AMN.vertexSet dcc
             in if vs `S.isSubsetOf` lets -- mandatory check to see if this scc belongs to this rank
                then
                  let newBindings = fmap (\ v -> over bindingSubterms (visitTerm curRank restDepthTable) ((letTable M.! v)^._3)) $ S.toList vs
                      (mAnn, mRecurs) = foldMap (\ v -> let b = letTable M.! v in (b^._1,b^._2)) vs
                  in case acc of
                       Let accAnn NonRec accBs accIn | mRecurs == NonRec ->
                         -- merge current let-group with previous let-group if both groups' recursivity is nonrec
                         Let (accAnn <> mAnn) NonRec (newBindings ++ accBs) accIn
                       _ ->
                         -- never merge if the previous let-group is a Rec or the current let-group is Rec,
                         -- but nest the current let-group under the previous let-group (above)
                         Let mAnn mRecurs newBindings acc
               else acc -- skip
             ) t topSortedSccs


-- Helpers
----------

toDepthData :: RankData -> DepthData
toDepthData = M.foldr (\ (LamDep lamDepth lamName, letNamePrinc) acc -> IM.insertWith (M.unionWith S.union) lamDepth (M.singleton lamName (S.singleton letNamePrinc)) acc) IM.empty

-- | Returns a single 'Unique' for a particular binding.
-- We need this because datatypebinds introduce multiple identifiers, but we need only one as a key of our 'RhsTable',etc. See also: 'datatypeIdentifiers'
--
-- TODO: maybe remove this boilerplate by having a lens "unique" for a Binding
bindingUnique :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique) => Binding tyname name a -> PLC.Unique
bindingUnique = \case TermBind _ _ (VarDecl _ n _) _ -> n ^. PLC.unique . coerced -- just the name coerced
                      TypeBind _ (TyVarDecl _ n _) _ -> n ^. PLC.unique . coerced -- just the name coerced
                      DatatypeBind _ (Datatype _ _ _ n _) -> n ^. PLC.unique . coerced -- arbitrary: uses the match-function's unique as the principal unique of this data binding group

