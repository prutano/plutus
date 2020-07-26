{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

-- | A top script allows *anyone* to redeem any value paid to the script

module Language.PlutusTx.Coordination.Contracts.Top where

import           Control.Lens
import           Control.Monad.Error.Lens

import qualified Data.Map                          as Map

import           Control.Monad                     (void)
import qualified Language.Plutus.Contract.Typed.Tx as Typed
import qualified Language.PlutusTx                 as PlutusTx
import           Ledger                            as Ledger hiding (initialise, to)
import qualified Ledger.Typed.Scripts              as Scripts

import           Language.Plutus.Contract          as Contract
import qualified Ledger.Constraints                as Constraints

data TopContract

instance Scripts.ScriptType TopContract where
  type RedeemerType TokenAccount = ()
  type DatumType TokenAccount = ()

topValidator :: () -> () -> ValidatorCtx -> Bool
topValidator _ _ _ = True

topScript :: Scripts.ScriptInstance TopContract
topScript = Scripts.validator @TopContract
  ($$(PlutusTx.compile [|| topValidator ||]))
  ($$(PlutusTx.compile [|| Scripts.wrapValidator @() @() ||]))

topAddress :: Address
topAddress = Scripts.scriptAddress topScript

data TopActionError =
    ScriptOutputMissing
    | MultipleScriptOutputs
    | OtherContractError ContractError
    deriving (Eq, Show)

makeClassyPrisms ''TopActionError

instance AsContractError TopActionError where
    _ContractError = _OtherContractError

type TopSchema =
    BlockchainActions
        .\/ Endpoint "pay" Value
        .\/ Endpoint "redeem" ()

payEndpoint :: Contract TopSchema TopActionError ()
payEndpoint = do
  val <- endpoint @"pay" @Value
  void $ payToTop val

redeemEndpoint :: Contract TopSchema TopActionError ()
redeemEndpoint = do
  () <- endpoint @"redeem"
  redeemAll

topEndpoint :: Contract TopSchema TopActionError ()
topEndpoint = payEndpoint `select` redeemEndpoint

findTxOut
  :: forall s e.
    ( AsTopActionError e )
  => Address -> Tx
  -> Contract s e (TxOutRef, TxOut)
findTxOut address tx =
  case output of
    [] ->
      throwing _ScriptOutputMissing ()
    [res] ->
      pure res
    _ ->
      throwing _MultipleScriptOutputs ()
 where
  output :: [(TxOutRef, TxOut)]
  output = Map.toList
          $ Map.filter ((==) address . txOutAddress)
          $ unspentOutputsTx tx

payToTop
  :: forall s e.
    ( HasWriteTx s
    , HasTxConfirmation s
    , AsTopActionError e
    )
  => Value
  -> Contract s e (Tx, TxOutRef, TxOut)
payToTop val = mapError (review _TopActionError) $ do
  tx <- submitTxConstraints topScript $
    Constraints.mustPayToTheScript () val

  void $ awaitTxConfirmed (txId tx)

  (txOutRef, txOut) <- findTxOut topAddress tx

  pure (tx, txOutRef, txOut)

redeemAll
  :: forall s e.
    ( HasWriteTx s
    , HasUtxoAt s
    , AsContractError e
    )
  => Contract s e ()
redeemAll = do
  txs <- utxoAt topAddress
  let tx = Typed.collectFromScript txs ()
  void $ submitTxConstraintsSpending topScript txs tx
