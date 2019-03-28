module MainFrame
  ( mainFrame
  , eval
  , initialState
  ) where

import Types

import Ace.Halogen.Component (AceEffects, AceMessage(TextChanged))
import Ace.Types (ACE, Annotation)
import Action (simulationPane)
import AjaxUtils (ajaxErrorPane, getDecodeJson)
import Analytics (Event, defaultEvent, trackEvent, ANALYTICS)
import Bootstrap (active, btn, btnGroup, btnSmall, col3_, col9_, container, container_, empty, hidden, navItem_, navLink, navTabs_, pullRight, row_)
import Chain (evaluationPane)
import Control.Bind (bindFlipped)
import Control.Comonad (extract)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.State (class MonadState)
import Control.Monad.Trans.Class (lift)
import Cursor (_current)
import Cursor as Cursor
import Data.Argonaut.Parser (jsonParser)
import Data.Array (catMaybes, (..))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Generic (gEq)
import Data.Lens (_1, _2, _Just, _Right, assign, modifying, over, set, traversed, use, view, to)
import Data.Lens.Extra (peruse)
import Data.Lens.Fold (maximumOf, preview)
import Data.Lens.Index (ix)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Editor (demoScriptsPane, editorPane)
import FileEvents (FILE)
import Gist (gistFileContent, gistId)
import Gists (gistControls, mkNewGist, playgroundGistFile, simulationGistFile)
import Gists as Gists
import Halogen (Component, action)
import Halogen as H
import Halogen.Component (ParentHTML)
import Halogen.ECharts (EChartsEffects)
import Halogen.ECharts as EC
import Halogen.HTML (ClassName(ClassName), HTML, a, br_, div, div_, h1, strong_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, href, id_)
import Halogen.Query (HalogenM)
import Icons (Icon(..), icon)
import Language.Haskell.Interpreter (CompilationError(CompilationError, RawError), InterpreterError(CompilationErrors, TimeoutError), _InterpreterResult)
import Ledger.Ada.TH (Ada(..))
import LocalStorage (LOCALSTORAGE)
import MonadApp (class MonadApp, editorGetContents, editorGotoLine, editorSetAnnotations, editorSetContents, getGistByGistId, getOauthStatus, patchGistByGistId, postContract, postEvaluation, postGist, preventDefault, readFileFromDragEvent, runHalogenApp, saveBuffer, updateChartsIfPossible)
import Network.HTTP.Affjax (AJAX)
import Network.RemoteData (RemoteData(NotAsked, Loading, Failure, Success), _Success, isSuccess)
import Playground.API (SimulatorWallet(SimulatorWallet), _CompilationResult, _FunctionSchema)
import Playground.Server (SPParams_)
import Prelude (type (~>), Unit, Void, bind, const, discard, flip, join, pure, show, unit, unless, when, ($), (&&), (+), (-), (<$>), (<*>), (<<<), (<>), (=<<), (==))
import Servant.PureScript.Settings (SPSettings_)
import StaticData as StaticData
import Wallet.Emulator.Types (Wallet(Wallet))

mkSimulatorWallet :: Int -> SimulatorWallet
mkSimulatorWallet id =
  SimulatorWallet { simulatorWalletWallet: Wallet { getWallet: id }
                  , simulatorWalletBalance: Ada {getAda: 10}
                  }

mkSimulation :: Signatures -> Simulation
mkSimulation signatures = Simulation
  { signatures
  , actions: []
  , wallets: mkSimulatorWallet <$> 1..2
  }

initialState :: State
initialState = State
  { currentView: Editor
  , compilationResult: NotAsked
  , simulations: Cursor.empty
  , evaluationResult: NotAsked
  , authStatus: NotAsked
  , createGistResult: NotAsked
  , gistUrl: Nothing
  }

------------------------------------------------------------

mainFrame ::
  forall m aff.
  MonadAff (EChartsEffects (AceEffects (ajax :: AJAX, analytics :: ANALYTICS, file :: FILE, localStorage :: LOCALSTORAGE | aff))) m
  => MonadAsk (SPSettings_ SPParams_) m
  => Component HTML Query Unit Void m
mainFrame =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval: evalWithAnalyticsTracking
    , receiver: const Nothing
    , initializer: Just $ H.action $ CheckAuthStatus
    , finalizer: Nothing
    }

evalWithAnalyticsTracking :: forall m eff aff.
  MonadEff (analytics :: ANALYTICS, ace :: ACE, localStorage :: LOCALSTORAGE | eff) m
  => MonadAsk (SPSettings_ SPParams_) m
  => MonadAff (ajax :: AJAX, file :: FILE | aff) m
  => Query ~> HalogenM State Query ChildQuery ChildSlot Void m
evalWithAnalyticsTracking query = do
  liftEff $ analyticsTracking query
  runHalogenApp $ eval query

analyticsTracking :: forall eff a. Query a -> Eff (analytics :: ANALYTICS | eff) Unit
analyticsTracking query = do
  case toEvent query of
    Nothing -> pure unit
    Just event -> trackEvent event

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
toEvent :: forall a. Query a -> Maybe Event
toEvent (HandleEditorMessage _ _) = Nothing
toEvent (HandleDragEvent _ _) = Nothing
toEvent (HandleDropEvent _ _) = Just $ defaultEvent "DropScript"
toEvent (HandleMockchainChartMessage _ _) = Nothing
toEvent (HandleBalancesChartMessage _ _) = Nothing
toEvent (CheckAuthStatus _) = Nothing
toEvent (PublishGist _) = Just $ (defaultEvent "Publish") { category = Just "Gist" }
toEvent (SetGistUrl _ _) = Nothing
toEvent (LoadGist _) = Just $ (defaultEvent "LoadGist") { category = Just "Gist" }
toEvent (ChangeView view _) = Just $ (defaultEvent "View") { label = Just $ show view }
toEvent (LoadScript script a) = Just $ (defaultEvent "LoadScript") { label = Just script }
toEvent (CompileProgram a) = Just $ defaultEvent "CompileProgram"
toEvent (ScrollTo _ _) = Nothing
toEvent (AddSimulationSlot _) = Just $ (defaultEvent "AddSimulationSlot") { category = Just "Simulation" }
toEvent (SetSimulationSlot _ _) = Just $ (defaultEvent "SetSimulationSlot") { category = Just "Simulation" }
toEvent (RemoveSimulationSlot _ _) = Just $ (defaultEvent "RemoveSimulationSlot") { category = Just "Simulation" }
toEvent (AddWallet _) = Just $ (defaultEvent "AddWallet") { category = Just "Wallet" }
toEvent (RemoveWallet _ _) = Just $ (defaultEvent "RemoveWallet") { category = Just "Wallet" }
toEvent (SetBalance _ _ _) = Just $ (defaultEvent "SetBalance") { category = Just "Wallet" }
toEvent (ModifyActions (AddAction _) _) = Just $ (defaultEvent "AddAction") { category = Just "Action" }
toEvent (ModifyActions (AddWaitAction _) _) = Just $ (defaultEvent "AddWaitAction") { category = Just "Action" }
toEvent (ModifyActions (RemoveAction _) _) = Just $ (defaultEvent "RemoveAction") { category = Just "Action" }
toEvent (ModifyActions (SetWaitTime _ _) _) = Just $ (defaultEvent "SetWaitTime") { category = Just "Action" }
toEvent (EvaluateActions _) = Just $ (defaultEvent "EvaluateActions") { category = Just "Action" }
toEvent (PopulateAction _ _ _) = Just $ (defaultEvent "PopulateAction") { category = Just "Action" }

eval ::
  forall m.
  MonadState State m
  => MonadAsk (SPSettings_ SPParams_) m
  => MonadApp m
  => Query ~> m
eval (HandleEditorMessage (TextChanged text) next) = do
  saveBuffer text
  pure next

eval (HandleDragEvent event next) = do
  preventDefault event
  pure next

eval (HandleDropEvent event next) = do
  preventDefault event
  contents <- readFileFromDragEvent event
  editorSetContents contents (Just 1)
  saveBuffer contents
  pure next

eval (HandleMockchainChartMessage EC.Initialized next) = do
  updateChartsIfPossible
  pure next

-- We just ignore most ECharts events.
eval (HandleMockchainChartMessage (EC.EventRaised event) next) =
  pure next

eval (HandleBalancesChartMessage EC.Initialized next) = do
  updateChartsIfPossible
  pure next

-- We just ignore most ECharts events.
eval (HandleBalancesChartMessage (EC.EventRaised event) next) =
  pure next

eval (CheckAuthStatus next) = do
  assign _authStatus Loading
  authResult <- getOauthStatus
  assign _authStatus authResult
  pure next

eval (PublishGist next) = do
  mContents <- editorGetContents
  simulations <- use _simulations
  mNewGist <- mkNewGist { source: mContents, simulations }
  case mNewGist of
    Nothing -> pure next
    Just newGist ->
      do mGist <- use _createGistResult

         assign _createGistResult Loading

         newResult <- case preview (_Success <<< gistId) mGist of
           Nothing -> postGist newGist
           Just gistId -> patchGistByGistId newGist gistId

         assign _createGistResult newResult

         case preview (_Success <<< gistId) newResult of
           Nothing -> pure unit
           Just gistId -> assign _gistUrl (Just (unwrap gistId))

         pure next

eval (SetGistUrl newGistUrl next) = do
  assign _gistUrl (Just newGistUrl)
  pure next

eval (LoadGist next) = do
  eGistId <- (bindFlipped Gists.parseGistUrl <<< note "Gist Url not set.") <$> use _gistUrl
  case eGistId of
    Left err -> pure unit
    Right gistId -> do
      assign _createGistResult Loading
      aGist <- getGistByGistId gistId
      assign _createGistResult aGist

      case aGist of
        Success gist -> do
          -- Load the source, if available.
          case preview (_Just <<< gistFileContent <<< _Just) (playgroundGistFile gist) of
            Nothing -> pure unit
            Just content -> do editorSetContents content (Just 1)
                               saveBuffer content
                               assign _simulations Cursor.empty
                               assign _evaluationResult NotAsked

          -- Load the simulation, if available.
          decodeJson <- getDecodeJson
          case preview (_Just <<< gistFileContent <<< _Just) (simulationGistFile gist) of
            Nothing -> pure unit
            Just simulationString -> do
              case (decodeJson =<< jsonParser simulationString) of
                Left err -> pure unit
                Right simulations -> do
                  assign _simulations simulations
                  assign _evaluationResult NotAsked

        _ -> pure unit

  pure next

eval (ChangeView view next) = do
  assign _currentView view
  pure next

eval (LoadScript key next) = do
  case Map.lookup key StaticData.demoFiles of
    Nothing -> pure next
    Just contents -> do
      editorSetContents contents (Just 1)
      saveBuffer contents
      assign _evaluationResult NotAsked
      assign _simulations Cursor.empty
      assign _compilationResult NotAsked
      pure next

eval (CompileProgram next) = do
  mContents <- editorGetContents

  case mContents of
    Nothing -> pure next
    Just contents -> do
      assign _compilationResult Loading
      result <- postContract contents
      assign _compilationResult result

      -- If we got a successful result, switch tab.
      case result of
        Success (Left _) -> pure unit
        _ -> replaceViewOnSuccess result Editor Simulations

      -- Update the error display.
      editorSetAnnotations $
        case result of
          Success (Left errors) -> toAnnotations errors
          _ -> []

      -- If we have a result with new signatures, we can only hold
      -- onto the old actions if the signatures still match. Any
      -- change means we'll have to clear out the existing simulation.
      case preview (_Success <<< _Right <<< _InterpreterResult <<< _result <<< _CompilationResult <<< _functionSchema) result of
        Just newSignatures -> do
          oldSignatures <- use (_simulations <<< _current <<< _signatures)
          unless (oldSignatures `gEq` newSignatures)
            (assign _simulations $ Cursor.singleton $ mkSimulation newSignatures)
        _ -> pure unit

      pure next

eval (ScrollTo {row, column} next) = do
  editorGotoLine row (Just column)
  pure next

eval (ModifyActions actionEvent next) = do
  modifying (_simulations <<< _current <<< _actions) (evalActionEvent actionEvent)
  pure next

eval (EvaluateActions next) = do
  _ <- runMaybeT $
       do evaluation <- MaybeT do
            contents <- editorGetContents
            simulation <- peruse (_simulations <<< _current)
            pure $ join $ toEvaluation <$> contents <*> simulation

          assign _evaluationResult Loading
          result <- lift $ postEvaluation evaluation
          assign _evaluationResult result

          replaceViewOnSuccess result Simulations Transactions

          lift updateChartsIfPossible

          pure unit
  pure next

eval (AddSimulationSlot next) = do
  mSignatures <- peruse (_compilationResult <<< _Success <<< _Right <<< _InterpreterResult <<< _result <<< _CompilationResult <<< _functionSchema)

  case mSignatures of
    Just signatures -> modifying _simulations (flip Cursor.snoc (mkSimulation signatures))
    Nothing -> pure unit
  pure next

eval (SetSimulationSlot index next) = do
  modifying _simulations (Cursor.setIndex index)
  pure next

eval (RemoveSimulationSlot index next) = do
  modifying _simulations (Cursor.deleteAt index)
  pure next

eval (AddWallet next) = do
  modifying (_simulations <<< _current <<< _wallets)
    (\wallets -> let maxWalletId = fromMaybe 0 $ maximumOf (traversed <<< _simulatorWalletWallet <<< _walletId) wallets
                     newWallet = mkSimulatorWallet (maxWalletId + 1)
                 in Array.snoc wallets newWallet)

  pure next

eval (RemoveWallet index next) = do
  modifying (_simulations <<< _current <<< _wallets) (fromMaybe <*> Array.deleteAt index)
  assign (_simulations <<< _current <<< _actions) []
  pure next

eval (SetBalance wallet newBalance next) = do
  modifying (_simulations <<< _current <<< _wallets <<< traversed)
    (\simulatorWallet -> if view _simulatorWalletWallet simulatorWallet == wallet
                         then set _simulatorWalletBalance newBalance simulatorWallet
                         else simulatorWallet)
  pure next

eval (PopulateAction n l event) = do
  modifying
    (_simulations
       <<< _current
       <<< _actions
       <<< ix n
       <<< _Action
       <<< _functionSchema
       <<< _FunctionSchema
       <<< _argumentSchema
       <<< ix l)
    (evalForm event)
  pure $ extract event

evalActionEvent :: ActionEvent -> Array Action -> Array Action
evalActionEvent (AddAction action) = flip Array.snoc action
evalActionEvent (AddWaitAction blocks) = flip Array.snoc (Wait { blocks })
evalActionEvent (RemoveAction index) = fromMaybe <*> Array.deleteAt index
evalActionEvent (SetWaitTime index time) = set (ix index <<< _Wait <<< _blocks) time

evalForm :: forall a. FormEvent a -> SimpleArgument -> SimpleArgument
evalForm (SetIntField n next) (SimpleInt _) = SimpleInt n
evalForm (SetIntField _ _) arg = arg

evalForm (SetStringField s next) (SimpleString _) = SimpleString (Just s)
evalForm (SetStringField _ _) arg = arg

evalForm (SetSubField 1 subEvent) (SimpleTuple fields) = SimpleTuple $ over _1 (evalForm subEvent) fields
evalForm (SetSubField 2 subEvent) (SimpleTuple fields) = SimpleTuple $ over _2 (evalForm subEvent) fields
evalForm (SetSubField _ subEvent) arg@(SimpleTuple fields) = arg
evalForm (SetSubField _ subEvent) arg@(SimpleString fields) = arg
evalForm (SetSubField _ subEvent) arg@(SimpleInt fields) = arg

evalForm (AddSubField _) (SimpleArray schema fields) =
  -- As the code stands, this is the only guarantee we get that every
  -- value in the array will conform to the schema: the fact that we
  -- create the 'empty' version from the same schema template.
  --
  -- Is more type safety than that possible? Probably.
  -- Is it worth the research effort? Perhaps. :thinking_face:
  SimpleArray schema $ Array.snoc fields (toValue schema)
evalForm (AddSubField _) arg = arg

evalForm (SetSubField n subEvent) (SimpleArray schema fields) =
  SimpleArray schema $ over (ix n) (evalForm subEvent) fields

evalForm (SetSubField n subEvent) s@(SimpleObject schema fields) =
  SimpleObject schema $ over (ix n <<< _2) (evalForm subEvent) fields
evalForm (SetSubField n subEvent) arg@(Unknowable _) = arg

evalForm (RemoveSubField n subEvent) arg@(SimpleArray schema fields ) =
  (SimpleArray schema (fromMaybe fields (Array.deleteAt n fields)))
evalForm (RemoveSubField n subEvent) arg =
  arg

replaceViewOnSuccess :: forall m e a. MonadState State m => RemoteData e a -> View -> View -> m Unit
replaceViewOnSuccess result source target = do
  currentView <- use _currentView
  when (isSuccess result && currentView == source)
    (assign _currentView target)

------------------------------------------------------------

toAnnotations :: InterpreterError -> Array Annotation
toAnnotations (TimeoutError _) = []
toAnnotations (CompilationErrors errors) = catMaybes (toAnnotation <$> errors)

toAnnotation :: CompilationError -> Maybe Annotation
toAnnotation (RawError _) = Nothing
toAnnotation (CompilationError {row, column, text}) =
  Just
    { type: "error"
    , row: row - 1
    , column
    , text: String.joinWith "\n" text
    }

render ::
  forall m aff.
  MonadAff (EChartsEffects (AceEffects (localStorage :: LOCALSTORAGE | aff))) m
  => State -> ParentHTML Query ChildQuery ChildSlot m
render state@(State {currentView})  =
  div
    [ class_ $ ClassName "main-frame" ]
    [ container_
        [ mainHeader
        , row_
            [ col9_ [ mainTabBar currentView
                    , br_
                    , demoScriptsPane
                    ]
            , col3_ [ gistControls state ]
            ]
        ]
    , viewContainer currentView Editor $
        [ editorPane defaultContents (view _compilationResult state)
        , case view _compilationResult state of
            Failure error -> ajaxErrorPane error
            _ -> empty
        ]
    , viewContainer currentView Simulations $
            [ simulationPane
                (view _simulations state)
                (view _evaluationResult state)
            , case (view _evaluationResult state) of
                Failure error -> ajaxErrorPane error
                _ -> empty
            ]
    , viewContainer currentView Transactions $
        case view _evaluationResult state of
          Success evaluation ->
            [ evaluationPane evaluation ]
          Failure error ->
            [ text "Your simulation has errors. Click the "
            , strong_ [ text "Simulation" ]
            , text " tab above to fix them and recompile."
            ]
          Loading -> [ icon Spinner ]
          NotAsked ->
            [ text "Click the "
            , strong_ [ text "Simulation"  ]
            , text " tab above and evaluate a simulation to see some results."
            ]
    ]
    where
      defaultContents = Map.lookup "Vesting" StaticData.demoFiles

viewContainer :: forall p i. View -> View -> Array (HTML p i) -> HTML p i
viewContainer currentView targetView =
  if currentView == targetView
  then div [ classes [ container ] ]
  else div [ classes [ container, hidden ] ]

mainHeader :: forall p. HTML p (Query Unit)
mainHeader =
  div_
    [ div [ classes [ btnGroup, pullRight ] ]
        (makeLink <$> links)
    , h1
        [ class_ $ ClassName "main-title" ]
        [ text "Plutus Playground" ]
    ]
  where
    links = [ Tuple "Getting Started" "https://testnet.iohkdev.io/plutus/get-started/writing-contracts-in-plutus/"
            , Tuple "Tutorial" "https://github.com/input-output-hk/plutus/blob/master/plutus-tutorial/tutorial/Tutorial/02-wallet-api.md"
            , Tuple "API" "https://input-output-hk.github.io/plutus/"
            , Tuple "Privacy" "https://static.iohk.io/docs/data-protection/iohk-data-protection-gdpr-policy.pdf"
            ]
    makeLink (Tuple name link) =
      a [ classes [ btn, btnSmall ]
        , href link
        ]
        [ text name ]

mainTabBar :: forall p. View -> HTML p (Query Unit)
mainTabBar activeView =
  navTabs_ (mkTab <$> tabs)
  where
    tabs = [ Editor /\ "Editor"
           , Simulations /\ "Simulation"
           , Transactions /\ "Transactions"
           ]

    mkTab :: Tuple View String -> HTML p (Query Unit)
    mkTab (link /\ title) =
      navItem_ [
        a
          [ id_ $ "tab-" <> String.toLower (show link)
          , classes $ [ navLink ] <> activeClass
          , onClick $ const $ Just $ action $ ChangeView link
          ]
          [ text title ]
      ]
      where
        activeClass =
          if link == activeView
          then [ active ]
          else []