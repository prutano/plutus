module TypeSpec where

import           Common
import           TestLib

import           Language.PlutusCore.Quote

import qualified Language.PlutusCore                         as PLC
import           Language.PlutusIR.Parser
import qualified Language.PlutusIR.Transform.LetFloat        as LetFloat
import qualified Language.PlutusIR.Transform.NonStrict       as NonStrict
import           Language.PlutusIR.Transform.Rename          ()
import qualified Language.PlutusIR.Transform.ThunkRecursions as ThunkRec

types :: TestNested
types = testNested "types"
    $ map (goldenTypeFromPir term)
  [ "letInLet"
  ,"listMatch"
  ,"maybe"
  ,"ifError"
  ,"mutuallyRecursiveTypes"
  ,"mutuallyRecursiveValues"
  ,"nonrec1"
  ,"nonrec2"
  ,"nonrec3"
  ,"nonrec4"
  ,"nonrec6"
  ,"nonrec7"
  ,"nonrec8"
  ,"rec1"
  ,"rec2"
  ,"rec3"
  ,"rec4"
  ,"nonrecToRec"
  ,"nonrecToNonrec"
  ,"oldLength"
  ,"strictValue"
  ,"strictNonValue"
  ,"strictNonValue2"
  ,"strictNonValue3"
  ,"strictValueNonValue"
  ,"strictValueValue"
  ,"even3Eval"
  ,"sameNameDifferentEnv"
  ,"bindDataTypeNormalize"
  ]

typeErrors :: TestNested
typeErrors = testNested "type-errors"
    $ map (goldenTypeFromPirCatch term)
    [ "conflictingRecursiveBinds"
    , "duplicateDataConstrs"
    , "duplicateTypeConstrs"
    , "wrongDataConstrReturnType"
    ,"nonSelfRecursive"
    ]
