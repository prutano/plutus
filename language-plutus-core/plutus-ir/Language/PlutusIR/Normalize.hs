{-# LANGUAGE FlexibleContexts #-}
-- | The user-facing API of the normalizer.

module Language.PlutusIR.Normalize
    ( Export.normalizeType
    , normalizeTypesIn
    , normalizeTypesInProgram
    ) where

import  Language.PlutusCore.Core as PLC (Normalized (..))
import           Language.PlutusCore.Name
import           Language.PlutusCore.Normalize.Internal hiding (normalizeTypesInM)
import           Language.PlutusCore.Quote
import Language.PlutusIR.Transform.Rename () -- for instances
import Language.PlutusCore.Rename (rename)
import Language.PlutusIR
import Language.PlutusCore.Normalize as Export (normalizeType)

import           Control.Lens
import           Control.Monad                          ((>=>))

-- | Normalize every 'Type' in a 'Term'.
normalizeTypesIn
    :: (HasUnique tyname TypeUnique, HasUnique name TermUnique, MonadQuote m)
    => Term tyname name uni ann -> m (Term tyname name uni ann)
normalizeTypesIn = rename >=> runNormalizeTypeM . normalizeTypesInM

-- | Normalize every 'Type' in a 'Program'.
normalizeTypesInProgram
    :: (HasUnique tyname TypeUnique, HasUnique name TermUnique, MonadQuote m)
    => Program tyname name uni ann -> m (Program tyname name uni ann)
normalizeTypesInProgram (Program x t) = Program x <$> normalizeTypesIn t

-- | Normalize every 'Type' in a 'Term'.
-- Overrides the similar `normalizeTypesInM` of 'Language.PlutusCore.Normalize.Internal'
normalizeTypesInM
    :: (HasUnique tyname TypeUnique, MonadQuote m)
    => Term tyname name uni ann -> NormalizeTypeT m tyname uni ann (Term tyname name uni ann)
normalizeTypesInM = transformMOf termSubterms normalizeChildTypes where
    normalizeChildTypes = termSubtypes (fmap unNormalized . normalizeTypeM)
