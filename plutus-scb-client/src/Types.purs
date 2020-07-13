module Types where

import Prelude
import Chain.Types as Chain
import Control.Monad.Gen as Gen
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Json.JsonMap (JsonMap)
import Data.Json.JsonUUID (JsonUUID, _JsonUUID)
import Data.Lens (Getter', Traversal', Lens', to, traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\))
import Data.UUID as UUID
import Foreign (MultipleErrors)
import Language.Plutus.Contract.Effects.ExposeEndpoint (ActiveEndpoint, EndpointDescription)
import Language.Plutus.Contract.Resumable (Request)
import Ledger.Index (UtxoIndex)
import Ledger.Tx (Tx)
import Ledger.TxId (TxId)
import Network.RemoteData (RemoteData)
import Playground.Types (FunctionSchema)
import Plutus.SCB.Events (ChainEvent)
import Plutus.SCB.Events.Contract (ContractInstanceId, ContractInstanceState, ContractSCBRequest, PartiallyDecodedResponse, _ContractInstanceState, _UserEndpointRequest)
import Plutus.SCB.Types (ContractExe)
import Plutus.SCB.Webserver.Types (ChainReport, ContractReport, ContractSignatureResponse, StreamToClient, StreamToServer, _ChainReport, _ContractReport, _ContractSignatureResponse)
import Schema (FormSchema)
import Schema.Types (FormArgument, FormEvent)
import Servant.PureScript.Ajax (AjaxError)
import Test.QuickCheck (class Arbitrary)
import Wallet.Rollup.Types (AnnotatedTx)
import WebSocket.Support as WS

data Query a
  = ReceiveWebSocketMessage (WS.Output StreamToClient) a

data Output
  = SendWebSocketMessage (WS.Input StreamToServer)

type WebData
  = RemoteData AjaxError

data HAction
  = Init
  | ChangeView View
  | LoadFullReport
  | ActivateContract ContractExe
  | ChainAction Chain.Action
  | ChangeContractEndpointCall ContractInstanceId Int FormEvent
  | InvokeContractEndpoint ContractInstanceId EndpointForm

newtype State
  = State
  { currentView :: View
  , contractSignatures :: WebData (Array (ContractSignatureResponse ContractExe))
  , chainReport :: WebData (ChainReport ContractExe)
  , events :: WebData (Array (ChainEvent ContractExe))
  , chainState :: Chain.State
  , contractStates :: Map ContractInstanceId (WebData (ContractInstanceState ContractExe /\ Array EndpointForm))
  , webSocketMessage :: RemoteData MultipleErrors StreamToClient
  }

type EndpointForm
  = { schema :: FunctionSchema FormSchema
    , argument :: FormArgument
    }

derive instance newtypeState :: Newtype State _

derive instance genericState :: Generic State _

_currentView :: Lens' State View
_currentView = _Newtype <<< prop (SProxy :: SProxy "currentView")

_contractSignatures :: forall s r a. Newtype s { contractSignatures :: a | r } => Lens' s a
_contractSignatures = _Newtype <<< prop (SProxy :: SProxy "contractSignatures")

_chainReport :: forall s r a. Newtype s { chainReport :: a | r } => Lens' s a
_chainReport = _Newtype <<< prop (SProxy :: SProxy "chainReport")

_events :: forall s r a. Newtype s { events :: a | r } => Lens' s a
_events = _Newtype <<< prop (SProxy :: SProxy "events")

_chainState :: Lens' State Chain.State
_chainState = _Newtype <<< prop (SProxy :: SProxy "chainState")

_contractStates :: Lens' State (Map ContractInstanceId (WebData (ContractInstanceState ContractExe /\ Array EndpointForm)))
_contractStates = _Newtype <<< prop (SProxy :: SProxy "contractStates")

_annotatedBlockchain :: forall t. Lens' (ChainReport t) (Array (Array AnnotatedTx))
_annotatedBlockchain = _ChainReport <<< prop (SProxy :: SProxy "annotatedBlockchain")

_transactionMap :: forall t. Lens' (ChainReport t) (JsonMap TxId Tx)
_transactionMap = _ChainReport <<< prop (SProxy :: SProxy "transactionMap")

_webSocketMessage :: forall s a r. Newtype s { webSocketMessage :: a | r } => Lens' s a
_webSocketMessage = _Newtype <<< prop (SProxy :: SProxy "webSocketMessage")

_contractReport :: forall s a r. Newtype s { contractReport :: a | r } => Lens' s a
_contractReport = _Newtype <<< prop (SProxy :: SProxy "contractReport")

_utxoIndex :: forall t. Lens' (ChainReport t) UtxoIndex
_utxoIndex = _ChainReport <<< prop (SProxy :: SProxy "utxoIndex")

_crAvailableContracts :: forall t. Lens' (ContractReport t) (Array (ContractSignatureResponse t))
_crAvailableContracts = _ContractReport <<< prop (SProxy :: SProxy "crAvailableContracts")

_crActiveContractStates :: forall t. Lens' (ContractReport t) (Array (ContractInstanceState t))
_crActiveContractStates = _ContractReport <<< prop (SProxy :: SProxy "crActiveContractStates")

_csrDefinition :: forall t. Lens' (ContractSignatureResponse t) t
_csrDefinition = _ContractSignatureResponse <<< prop (SProxy :: SProxy "csrDefinition")

_csContract :: forall t. Lens' (ContractInstanceState t) ContractInstanceId
_csContract = _Newtype <<< prop (SProxy :: SProxy "csContract")

_csCurrentState :: forall t. Lens' (ContractInstanceState t) (PartiallyDecodedResponse ContractSCBRequest)
_csCurrentState = _Newtype <<< prop (SProxy :: SProxy "csCurrentState")

_csContractDefinition :: forall t. Lens' (ContractInstanceState t) t
_csContractDefinition = _ContractInstanceState <<< prop (SProxy :: SProxy "csContractDefinition")

_hooks :: forall t. Lens' (PartiallyDecodedResponse t) (Array (Request t))
_hooks = _Newtype <<< prop (SProxy :: SProxy "hooks")

_activeEndpoint :: Lens' ActiveEndpoint EndpointDescription
_activeEndpoint = _Newtype <<< prop (SProxy :: SProxy "aeDescription")

_contractActiveEndpoints :: Traversal' (PartiallyDecodedResponse ContractSCBRequest) EndpointDescription
_contractActiveEndpoints =
  _hooks
    <<< traversed
    <<< _rqRequest
    <<< _UserEndpointRequest
    <<< _activeEndpoint

_rqRequest :: forall t. Lens' (Request t) t
_rqRequest = _Newtype <<< prop (SProxy :: SProxy "rqRequest")

_contractPath :: Lens' ContractExe String
_contractPath = _Newtype <<< prop (SProxy :: SProxy "contractPath")

_contractInstanceId :: Lens' ContractInstanceId JsonUUID
_contractInstanceId = _Newtype <<< prop (SProxy :: SProxy "unContractInstanceId")

_contractInstanceIdString :: Getter' ContractInstanceId String
_contractInstanceIdString = _contractInstanceId <<< _JsonUUID <<< to UUID.toString

------------------------------------------------------------
data View
  = ActiveContracts
  | Blockchain
  | EventLog

derive instance eqView :: Eq View

derive instance genericView :: Generic View _

instance arbitraryView :: Arbitrary View where
  arbitrary = Gen.elements (ActiveContracts :| [ Blockchain, EventLog ])

instance showView :: Show View where
  show = genericShow
