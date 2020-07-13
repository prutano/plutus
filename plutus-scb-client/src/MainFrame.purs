module MainFrame
  ( initialMainFrame
  , handleQuery
  , handleAction
  , initialState
  ) where

import Prelude hiding (div)
import Animation (class MonadAnimate, animate)
import Chain.Eval (handleAction) as Chain
import Chain.Types (Action(..), AnnotatedBlockchain(..), _chainFocusAppearing)
import Chain.Types (initialState) as Chain
import Clipboard (class MonadClipboard)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (class MonadState)
import Control.Monad.State.Extra (zoomStateT)
import Data.Array (filter, find)
import Data.Either (Either(..))
import Data.Lens (_1, _2, assign, modifying, to, use, view)
import Data.Lens.At (at)
import Data.Lens.Extra (peruse, toSetOf)
import Data.Lens.Index (ix)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.RawJson (RawJson(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for_, sequence, traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Foreign.Generic (encodeJSON)
import Halogen (Component, hoist)
import Halogen as H
import Halogen.HTML (HTML)
import Language.Plutus.Contract.Effects.ExposeEndpoint (EndpointDescription)
import Ledger.Ada (Ada(..))
import Ledger.Extra (adaToValue)
import Ledger.Value (Value)
import MonadApp (class MonadApp, activateContract, getFullReport, invokeEndpoint, log, runHalogenApp)
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Playground.Lenses (_endpointDescription, _schema)
import Playground.Types (FunctionSchema(..), _FunctionSchema)
import Plutus.SCB.Events.Contract (ContractInstanceState(..))
import Plutus.SCB.Types (ContractExe)
import Plutus.SCB.Webserver (SPParams_(..))
import Plutus.SCB.Webserver.Types (ContractSignatureResponse(..), StreamToClient(..))
import Prim.TypeError (class Warn, Text)
import Schema (FormSchema)
import Schema.Types (formArgumentToJson, toArgument)
import Schema.Types as Schema
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Types (EndpointForm, HAction(..), Output, Query(..), State(..), View(..), WebData, _annotatedBlockchain, _chainReport, _chainState, _contractActiveEndpoints, _contractReport, _contractSignatures, _contractStates, _crActiveContractStates, _crAvailableContracts, _csContract, _csCurrentState, _currentView, _events, _webSocketMessage)
import Validation (_argument)
import View as View
import WebSocket.Support as WS

initialValue :: Value
initialValue = adaToValue $ Lovelace { getLovelace: 0 }

initialState :: State
initialState =
  State
    { currentView: ActiveContracts
    , contractSignatures: NotAsked
    , chainReport: NotAsked
    , events: NotAsked
    , chainState: Chain.initialState
    , contractStates: Map.empty
    , webSocketMessage: NotAsked
    }

------------------------------------------------------------
ajaxSettings :: SPSettings_ SPParams_
ajaxSettings = defaultSettings $ SPParams_ { baseURL: "/" }

initialMainFrame ::
  forall m.
  MonadAff m =>
  MonadClipboard m =>
  Component HTML Query HAction Output m
initialMainFrame =
  hoist (flip runReaderT ajaxSettings)
    $ H.mkComponent
        { initialState: const initialState
        , render: View.render
        , eval:
          H.mkEval
            { handleAction: runHalogenApp <<< handleAction
            , handleQuery: runHalogenApp <<< handleQuery
            , initialize: Just Init
            , receive: const Nothing
            , finalize: Nothing
            }
        }

handleQuery ::
  forall m a.
  Warn (Text "Handle WebSocket errors.") =>
  Warn (Text "Handle WebSocket disconnections.") =>
  MonadState State m =>
  MonadApp m =>
  Query a -> m (Maybe a)
handleQuery (ReceiveWebSocketMessage (WS.ReceiveMessage msg) next) = do
  case msg of
    Right (NewChainReport report) -> assign (_chainReport <<< _Success) report
    Right (NewContractReport report) -> do
      assign (_contractSignatures <<< _Success) (view _crAvailableContracts report)
      traverse_ updateFormsForContractInstance
        (view _crActiveContractStates report)
    Right (NewChainEvents events) -> assign (_events <<< _Success) events
    Right (ErrorResponse _) -> pure unit
    Left err -> pure unit
  assign _webSocketMessage $ RemoteData.fromEither msg
  pure $ Just next

handleQuery (ReceiveWebSocketMessage WS.WebSocketClosed next) = do
  log "Closed"
  pure $ Just next

handleAction ::
  forall m.
  MonadApp m =>
  MonadAnimate m State =>
  MonadClipboard m =>
  MonadState State m =>
  HAction -> m Unit
handleAction Init = handleAction LoadFullReport

handleAction (ChangeView view) = assign _currentView view

handleAction (ActivateContract contract) = activateContract contract

handleAction LoadFullReport = do
  assignFullReportData Loading
  fullReportResult <- getFullReport
  assignFullReportData fullReportResult
  for_ fullReportResult
    ( \report ->
        traverse_ updateFormsForContractInstance
          (view (_contractReport <<< _crActiveContractStates) report)
    )
  where
  assignFullReportData v = do
    assign _contractSignatures (view (_contractReport <<< _crAvailableContracts) <$> v)
    assign _chainReport (view _chainReport <$> v)
    assign _events (view _events <$> v)

handleAction (ChainAction subaction) = do
  mAnnotatedBlockchain <-
    peruse (_chainReport <<< _Success <<< _annotatedBlockchain <<< to AnnotatedBlockchain)
  let
    wrapper ::
      Warn (Text "The question, 'Should we animate this?' feels like it belongs in the Chain module. Not here.") =>
      m Unit -> m Unit
    wrapper = case subaction of
      (FocusTx _) -> animate (_chainState <<< _chainFocusAppearing)
      _ -> identity
  wrapper
    $ zoomStateT _chainState
    $ Chain.handleAction subaction mAnnotatedBlockchain

handleAction (ChangeContractEndpointCall contractInstanceId endpointIndex subaction) = do
  modifying
    ( _contractStates
        <<< ix contractInstanceId
        <<< _Success
        <<< _2
        <<< ix endpointIndex
        <<< _argument
    )
    (Schema.handleFormEvent initialValue subaction)

handleAction (InvokeContractEndpoint contractInstanceId endpointForm) = do
  let
    endpointDescription :: EndpointDescription
    endpointDescription = view (_schema <<< _FunctionSchema <<< _endpointDescription) endpointForm

    encodedForm :: Maybe RawJson
    encodedForm = RawJson <<< encodeJSON <$> formArgumentToJson (view _argument endpointForm)
  for_ encodedForm
    $ \argument -> do
        assign (_contractStates <<< at contractInstanceId) (Just Loading)
        invokeEndpoint argument contractInstanceId endpointDescription

updateFormsForContractInstance ::
  forall m.
  MonadState State m =>
  ContractInstanceState ContractExe -> m Unit
updateFormsForContractInstance newContractInstance = do
  let
    csContractId = view _csContract newContractInstance
  oldContractInstance :: Maybe (ContractInstanceState ContractExe) <-
    peruse
      ( _contractStates
          <<< ix csContractId
          <<< _Success
          <<< _1
      )
  when (oldContractInstance /= Just newContractInstance)
    $ do
        contractSignatures <- use _contractSignatures
        let
          newForms :: Maybe (WebData (Array EndpointForm))
          newForms = sequence $ createNewEndpointForms <$> contractSignatures <*> pure newContractInstance
        assign (_contractStates <<< at csContractId)
          (map (Tuple newContractInstance) <$> newForms)

createNewEndpointForms ::
  Array (ContractSignatureResponse ContractExe) ->
  ContractInstanceState ContractExe ->
  Maybe (Array EndpointForm)
createNewEndpointForms contractSignatures instanceState =
  let
    matchingSignature :: Maybe (ContractSignatureResponse ContractExe)
    matchingSignature = getMatchingSignature instanceState contractSignatures
  in
    createEndpointForms instanceState <$> matchingSignature

createEndpointForms ::
  forall t.
  ContractInstanceState t ->
  ContractSignatureResponse t ->
  Array EndpointForm
createEndpointForms contractState = signatureToForms
  where
  activeEndpoints :: Set EndpointDescription
  activeEndpoints =
    toSetOf
      ( _csCurrentState
          <<< _contractActiveEndpoints
      )
      contractState

  isActive :: FunctionSchema FormSchema -> Boolean
  isActive (FunctionSchema { endpointDescription }) = Set.member endpointDescription activeEndpoints

  signatureToForms :: ContractSignatureResponse t -> Array EndpointForm
  signatureToForms (ContractSignatureResponse { csrSchemas }) = signatureToForm <$> filter isActive csrSchemas

  signatureToForm :: FunctionSchema FormSchema -> EndpointForm
  signatureToForm schema =
    { argument: toArgument initialValue $ view (_FunctionSchema <<< _argument) schema
    , schema
    }

getMatchingSignature ::
  forall t.
  Eq t =>
  ContractInstanceState t ->
  Array (ContractSignatureResponse t) ->
  Maybe (ContractSignatureResponse t)
getMatchingSignature (ContractInstanceState { csContractDefinition }) = find isMatch
  where
  isMatch (ContractSignatureResponse { csrDefinition }) = csrDefinition == csContractDefinition
