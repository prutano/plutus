{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Language.PlutusIR.Compiler.Error
    ( Error (..)
    , TypeError (..)
    , AsTypeError (..)
    , AsError (..)
    , PLC.Normalized (..)
    , PLC.UnknownDynamicBuiltinNameError (..)
    , throwingEither
    ) where

import qualified Language.PlutusCore        as PLC
import qualified Language.PlutusCore.Pretty as PLC
--import qualified Language.PlutusCore.Error as PLC (throwingEither)
import qualified Language.PlutusIR as PIR

import           Control.Exception hiding (TypeError)
import           Control.Lens

import qualified Data.Text                  as T
import Data.Text.Prettyprint.Doc  as PP
import           Data.Typeable
import GHC.Generics (Generic)
import Control.Monad.Error.Lens
import Control.Monad.Except


-- | Lifts an 'Either' into an error context where we can embed the 'Left' value into the error.
-- TODO: literally copy-pasted from PlutusCore.Error to avoid modifying the exposed modules/functions of language-plutus-core package
throwingEither :: MonadError e m => AReview e t -> Either t a -> m a
throwingEither r e = case e of
    Left t  -> throwing r t
    Right v -> pure v


data TypeError uni ann
    = KindMismatch ann (PLC.Type PLC.TyName uni ()) (PLC.Kind ())  (PLC.Kind ())
    | TypeMismatch ann
        (PIR.Term PLC.TyName PLC.Name uni ())
        (PLC.Type PLC.TyName uni ())
        (PLC.Normalized (PLC.Type PLC.TyName uni ()))
    | UnknownDynamicBuiltinName ann PLC.UnknownDynamicBuiltinNameError
    | InternalTypeErrorE ann (PLC.InternalTypeError uni ann)
    | FreeTypeVariableE ann PLC.TyName
    | FreeVariableE ann PLC.Name
    | ConflictingRecBinds ann T.Text
    | DuplicateDeclaredIdent ann T.Text
    | MalformedDataConstrResType ann (PLC.Type PLC.TyName uni ())  --  expected
    -- TODO: enable back deriving NFData because PIR.Term does not have it
    -- TODO: enable back deriving Eq because PIR.Term does not have it, see similar src/Language/PlutusCore/Core/Instance/Eq.hs
    deriving (Show, Generic) 
makeClassyPrisms ''TypeError


data Error uni a = CompilationError a T.Text -- ^ A generic compilation error.
                 | UnsupportedError a T.Text -- ^ An error relating specifically to an unsupported feature.
                 | PLCError (PLC.Error uni a) -- ^ An error from running some PLC function, lifted into this error type for convenience.
                 | TypeErrorE (TypeError uni a)
               deriving (Typeable)
makeClassyPrisms ''Error



instance PLC.AsTypeError (Error uni a) uni a where
    _TypeError = _PLCError . PLC._TypeError

instance AsTypeError (Error uni a) uni a where
    _TypeError = _TypeErrorE

instance (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty a) =>
            Show (Error uni a) where
    show e = show $ PLC.prettyPlcClassicDebug e

instance (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty a) =>
            PLC.PrettyBy PLC.PrettyConfigPlc (Error uni a) where
    prettyBy config = \case
        CompilationError x e -> "Error during compilation:" <+> PP.pretty e <> "(" <> PP.pretty x <> ")"
        UnsupportedError x e -> "Unsupported construct:" <+> PP.pretty e <+> "(" <> PP.pretty x <> ")"
        PLCError e -> PP.vsep [ "Error from the PLC compiler:", PLC.prettyBy config e ]
        TypeErrorE e -> PP.vsep ["Error during typechecking:" , PLC.prettyBy config e ]

-- NOTE: adapted from `plutus-core/Error.hs`
instance (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst,  PP.Pretty ann) =>
            PLC.PrettyBy PLC.PrettyConfigPlc (TypeError uni ann) where
    prettyBy config (KindMismatch ann ty k k')          =
        "Kind mismatch at" <+> pretty ann <+>
        "in type" <+> squotes (PLC.prettyBy config ty) <>
        ". Expected kind" <+> squotes (PLC.prettyBy config k) <+>
        ", found kind" <+> squotes (PLC.prettyBy config k')
    prettyBy config (TypeMismatch ann t ty ty')         =
        "Type mismatch at" <+> pretty ann <>
        (if PLC._pcpoCondensedErrors (PLC._pcpOptions config) == PLC.CondensedErrorsYes
            then mempty
            else " in term" <> hardline <> indent 2 (squotes (PLC.prettyClassicDef t)) <> ".") <>
        hardline <>
        "Expected type" <> hardline <> indent 2 (squotes (PLC.prettyBy config ty)) <>
        "," <> hardline <>
        "found type" <> hardline <> indent 2 (squotes (PLC.prettyBy config ty'))
    prettyBy config (FreeTypeVariableE ann name)          =
        "Free type variable at " <+> pretty ann <+> ": " <+> PLC.prettyBy config name
    prettyBy config (FreeVariableE ann name)              =
        "Free variable at " <+> pretty ann <+> ": " <+> PLC.prettyBy config name
    prettyBy config (InternalTypeErrorE ann err)        =
        PLC.prettyBy config err <> hardline <>
        "Error location:" <+> pretty ann
    prettyBy _      (UnknownDynamicBuiltinName ann err) =
        "Unknown dynamic built-in name at" <+> pretty ann <>
        ":" <+> pretty err
    prettyBy _ (ConflictingRecBinds ann err) = 
        vsep ["Some binds of a recursive-let conflict with each other at" <+> PP.pretty ann
             , "Conflicting identifier:" <+> pretty err]
    prettyBy _ (DuplicateDeclaredIdent ann err) =
        vsep ["A duplicate identifier is introduced inside a single datatype-bind of let at" <+> PP.pretty ann
             , "Offending identifier:" <+> pretty err]
    prettyBy _ (MalformedDataConstrResType ann expType) =
        vsep ["The result-type of a dataconstructor is malformed at location" <+> PP.pretty ann
             , "The expected result-type is:" <+> PP.pretty expType]


instance ( PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty a
         , Typeable uni, Typeable a
         ) => Exception (Error uni a)
