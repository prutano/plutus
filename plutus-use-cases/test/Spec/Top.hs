{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Spec.Top where

import           Language.Plutus.Contract.Test
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                   as Ada

import           Language.PlutusTx.Coordination.Contracts.Top

import qualified Language.PlutusTx.Numeric                    as Numeric
import           Test.Tasty

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

tests :: TestTree
tests = testGroup "pubkey"
  [
    checkPredicate "Expose 'pay' and 'redeem' endpoints"
      topEndpoint
      (endpointAvailable @"pay" w1 /\ endpointAvailable @"redeem" w1)
      $ pure ()

  , checkPredicate "w1 pays 10 Ada"
      topEndpoint
      (walletFundsChange w1 (Numeric.negate $ Ada.lovelaceValueOf 10))
      $ do
        callEndpoint @"pay" w1 (Ada.lovelaceValueOf 10)
        handleBlockchainEvents w1
        addBlocks 1

  , checkPredicate "w2 can redeem 10 Ada"
      topEndpoint
      (walletFundsChange w1 (Numeric.negate $ Ada.lovelaceValueOf 10)
        /\ walletFundsChange w2 (Ada.lovelaceValueOf 10))
      $ do
        callEndpoint @"pay" w1 (Ada.lovelaceValueOf 10)
        handleBlockchainEvents w1
        addBlocks 1

        callEndpoint @"redeem" w2 ()
        handleBlockchainEvents w2
        addBlocks 1
  ]
