module View (render) where

import Bootstrap (col12_, col5_, col7_, container_, row_)
import Chain.Types as Chain
import Data.Lens (traversed, view)
import Data.Lens.Extra (toArrayOf)
import Data.Map (Map)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), tuple3, uncurry3)
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML (ClassName(..), ComponentHTML, HTML, div, div_, h1, text)
import Halogen.HTML.Properties (class_)
import NavTabs (mainTabBar, viewContainer)
import Plutus.SCB.Events (ChainEvent)
import Plutus.SCB.Events.Contract (ContractInstanceId, ContractInstanceState)
import Plutus.SCB.Types (ContractExe)
import Plutus.SCB.Webserver.Types (ChainReport, ContractReport, ContractSignatureResponse(..))
import Prelude (($), (<$>), (<*>), (<<<))
import Types (EndpointForm, HAction(..), State(..), View(..), WebData, _crAvailableContracts, _csrDefinition, _utxoIndex)
import View.Blockchain (annotatedBlockchainPane)
import View.Contracts (contractStatusesPane, installedContractsPane)
import View.Events (eventsPane, utxoIndexPane)
import View.Utils (webDataPane)

render ::
  forall m slots.
  MonadAff m =>
  State -> ComponentHTML HAction slots m
render (State { currentView, chainState, contractSignatures, chainReport, events, contractStates, webSocketMessage }) =
  div
    [ class_ $ ClassName "main-frame" ]
    [ container_
        [ mainHeader
        , mainTabBar ChangeView tabs currentView
        , div_
            $ webDataPane
                ( uncurry3
                    (mainPane currentView contractStates chainState)
                )
                (tuple3 <$> contractSignatures <*> chainReport <*> events)
        ]
    ]

mainHeader :: forall p. HTML p HAction
mainHeader =
  div_
    [ h1
        [ class_ $ ClassName "main-title" ]
        [ text "Plutus Application Platform" ]
    ]

tabs :: Array { help :: String, link :: View, title :: String }
tabs =
  [ { link: ActiveContracts
    , title: "Contracts"
    , help: "See available and active contracts."
    }
  , { link: Blockchain
    , title: "Blockchain"
    , help: "See the current state of the chain."
    }
  , { link: EventLog
    , title: "Event Log"
    , help: "View the history of system events."
    }
  ]

mainPane ::
  forall p t.
  View ->
  Map ContractInstanceId (WebData (ContractInstanceState t /\ Array EndpointForm)) ->
  Chain.State ->
  Array (ContractSignatureResponse ContractExe) ->
  ChainReport t ->
  Array (ChainEvent ContractExe) ->
  HTML p HAction
mainPane currentView contractStates chainState contractSignatures chainReport events =
  row_
    [ activeContractPane currentView contractStates contractSignatures
    , blockchainPane currentView chainState chainReport
    , eventLogPane currentView events chainReport
    ]

activeContractPane ::
  forall p t.
  View ->
  Map ContractInstanceId
    ( WebData
        ( Tuple (ContractInstanceState t)
            ( Array
                EndpointForm
            )
        )
    ) ->
  Array (ContractSignatureResponse ContractExe) -> HTML p HAction
activeContractPane currentView contractStates contractSignatures =
  viewContainer currentView ActiveContracts
    [ row_
        [ col12_ [ contractStatusesPane contractStates ]
        , col12_
            [ installedContractsPane
                ( toArrayOf
                    ( traversed
                        <<< _csrDefinition
                    )
                    contractSignatures
                )
            ]
        ]
    ]

blockchainPane ::
  forall p t.
  View ->
  Chain.State ->
  ChainReport t -> HTML p HAction
blockchainPane currentView chainState chainReport =
  viewContainer currentView Blockchain
    [ row_
        [ col12_ [ ChainAction <$> annotatedBlockchainPane chainState chainReport ]
        ]
    ]

eventLogPane :: forall p t. View -> Array (ChainEvent ContractExe) -> ChainReport t -> HTML p HAction
eventLogPane currentView events chainReport =
  viewContainer currentView EventLog
    [ row_
        [ col7_ [ eventsPane events ]
        , col5_ [ utxoIndexPane (view _utxoIndex chainReport) ]
        ]
    ]
