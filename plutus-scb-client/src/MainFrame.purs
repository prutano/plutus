module MainFrame
  ( initialMainFrame
  , handleAction
  , initialState
  ) where

import Prelude hiding (div)
import Animation (animate)
import Chain.Eval (handleAction) as Chain
import Chain.Types (Action(..), AnnotatedBlockchain(..), _chainFocusAppearing)
import Chain.Types (initialState) as Chain
import Clipboard (class MonadClipboard)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, runReaderT)
import Control.Monad.State (class MonadState)
import Control.Monad.State.Extra (zoomStateT)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens (_1, _2, assign, findOf, modifying, to, traversed, view)
import Data.Lens.At (at)
import Data.Lens.Extra (peruse, toSetOf)
import Data.Lens.Index (ix)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.RawJson (RawJson(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Foreign.Generic (encodeJSON)
import Halogen (Component, HalogenM, hoist, raise)
import Halogen as H
import Halogen.HTML (HTML)
import Language.Plutus.Contract.Effects.ExposeEndpoint (EndpointDescription)
import Ledger.Ada (Ada(..))
import Ledger.Extra (adaToValue)
import Ledger.Value (Value)
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Playground.Lenses (_endpointDescription, _getEndpointDescription, _schema)
import Playground.Types (FunctionSchema(..), _FunctionSchema)
import Plutus.SCB.Events.Contract (ContractInstanceState(..))
import Plutus.SCB.Types (ContractExe)
import Plutus.SCB.Webserver (SPParams_(..), getApiContractByContractinstanceidSchema, getApiFullreport, postApiContractActivate, postApiContractByContractinstanceidEndpointByEndpointname)
import Plutus.SCB.Webserver.Types (ContractSignatureResponse(..), FullReport, StreamToClient(..), StreamToServer(..))
import Prim.TypeError (class Warn, Text)
import Schema (FormSchema)
import Schema.Types (formArgumentToJson, toArgument)
import Schema.Types as Schema
import Servant.PureScript.Ajax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Types (EndpointForm, HAction(..), Output(..), Query(..), State(..), View(..), WebData, _annotatedBlockchain, _chainReport, _chainState, _contractActiveEndpoints, _contractInstanceIdString, _contractReport, _contractSignatures, _contractStates, _crAvailableContracts, _csContract, _csCurrentState, _currentView, _events, _fullReport, _webSocketMessage)
import Validation (_argument)
import View as View
import WebSocket.Support as WS

initialValue :: Value
initialValue = adaToValue $ Lovelace { getLovelace: 0 }

initialState :: State
initialState =
  State
    { currentView: ActiveContracts
    , fullReport: NotAsked
    , chainState: Chain.initialState
    , contractSignatures: Map.empty
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
            { handleAction
            , handleQuery
            , initialize: Just Init
            , receive: const Nothing
            , finalize: Nothing
            }
        }

handleQuery ::
  forall m a.
  Warn (Text "Handle WebSocket errors.") =>
  Warn (Text "Handle WebSocket disconnections.") =>
  MonadAff m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadState State m =>
  MonadEffect m =>
  Query a -> m (Maybe a)
handleQuery (ReceiveWebSocketMessage (WS.ReceiveMessage msg) next) = do
  case msg of
    Right (NewChainReport report) -> assign (_fullReport <<< _Success <<< _chainReport) report
    Right (NewContractReport report) -> do
      assign (_fullReport <<< _Success <<< _contractReport) report
      traverse_ updateFormsForContractInstance
        (view _contractStates report)
    Right (NewChainEvents events) -> assign (_fullReport <<< _Success <<< _events) events
    Right (Echo _) -> pure unit
    Right (ErrorResponse _) -> pure unit
    Left err -> pure unit
  assign _webSocketMessage $ RemoteData.fromEither msg
  pure $ Just next

handleQuery (ReceiveWebSocketMessage WS.WebSocketClosed next) = do
  liftEffect $ log "Closed"
  pure $ Just next

handleAction ::
  forall action slots m.
  MonadAff m =>
  MonadClipboard m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadEffect m =>
  HAction -> HalogenM State action slots Output m Unit
handleAction Init = handleAction LoadFullReport

handleAction (ChangeView view) = do
  sendWebSocketMessage $ Ping $ show view
  assign _currentView view

handleAction (ActivateContract contract) = void $ runAjax $ postApiContractActivate contract

handleAction LoadFullReport = do
  assign _fullReport Loading
  fullReportResult <- runAjax getApiFullreport
  assign _fullReport fullReportResult
  for_ fullReportResult
    ( \fullReport ->
        traverse_ updateFormsForContractInstance
          (view (_contractReport <<< _contractStates) fullReport)
    )

handleAction (ChainAction subaction) = do
  mAnnotatedBlockchain <-
    peruse (_fullReport <<< _Success <<< _chainReport <<< _annotatedBlockchain <<< to AnnotatedBlockchain)
  let
    wrapper ::
      Warn (Text "The question, 'Should we animate this?' feels like it belongs in the Chain module. Not here.") =>
      HalogenM State action slots Output m Unit -> HalogenM State action slots Output m Unit
    wrapper = case subaction of
      (FocusTx _) -> animate (_chainState <<< _chainFocusAppearing)
      _ -> identity
  wrapper
    $ zoomStateT _chainState
    $ Chain.handleAction subaction mAnnotatedBlockchain

handleAction (ChangeContractEndpointCall contractInstanceId endpointIndex subaction) = do
  modifying
    ( _contractSignatures
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
        assign (_contractSignatures <<< at contractInstanceId) (Just Loading)
        runAjax
          $ let
              instanceId = view _contractInstanceIdString contractInstanceId

              endpoint = view _getEndpointDescription endpointDescription
            in
              postApiContractByContractinstanceidEndpointByEndpointname argument instanceId endpoint

updateFormsForContractInstance ::
  forall m.
  Warn (Text "TODO We shouldn't have to go to the backend every time for this data. Contract schemas don't change during the lifetime of the contract.") =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadAff m =>
  MonadState State m =>
  ContractInstanceState ContractExe -> m Unit
updateFormsForContractInstance newContractInstance = do
  let
    csContractId = view _csContract newContractInstance
  oldContractInstance :: Maybe (ContractInstanceState ContractExe) <-
    peruse
      ( _contractSignatures
          <<< ix csContractId
          <<< _Success
          <<< _1
      )
  when (oldContractInstance /= Just newContractInstance)
    $ do
        let
          uuid = view (_csContract <<< _contractInstanceIdString) newContractInstance
        contractSchema <- runAjax $ getApiContractByContractinstanceidSchema uuid
        assign (_contractSignatures <<< at csContractId)
          (Just (Tuple newContractInstance <$> (createEndpointForms newContractInstance <$> contractSchema)))

createNewEndpointFormsM ::
  forall m.
  Monad m =>
  m (FullReport ContractExe) ->
  m (ContractInstanceState ContractExe) ->
  m (Maybe (Array EndpointForm))
createNewEndpointFormsM mFullReport mInstanceState = do
  fullReport <- mFullReport
  instanceState <- mInstanceState
  let
    matchingSignature :: Maybe (ContractSignatureResponse ContractExe)
    matchingSignature = getMatchingSignature instanceState fullReport

    newForms :: Maybe (Array EndpointForm)
    newForms = createEndpointForms instanceState <$> matchingSignature
  pure newForms

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
  FullReport t ->
  Maybe (ContractSignatureResponse t)
getMatchingSignature (ContractInstanceState { csContractDefinition }) =
  findOf
    ( _contractReport
        <<< _crAvailableContracts
        <<< traversed
    )
    isMatch
  where
  isMatch (ContractSignatureResponse { csrDefinition }) = csrDefinition == csContractDefinition

runAjax :: forall m a. Functor m => ExceptT AjaxError m a -> m (WebData a)
runAjax action = RemoteData.fromEither <$> runExceptT action

sendWebSocketMessage :: forall state action slots m. StreamToServer ContractExe -> HalogenM state action slots Output m Unit
sendWebSocketMessage msg = raise $ SendWebSocketMessage $ WS.SendMessage msg
