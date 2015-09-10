{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  ShiftGame.Main
-- Copyright   :  (c) 2015, Thomas Pajenkamp
-- License     :  BSD3
--
-- Maintainer  :  tpajenka
-- Stability   :
-- Portability :
--
-- | Entry point for setting up the game
--
-----------------------------------------------------------------------------

module Main where

import           Control.Concurrent
import           Control.Applicative
--import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Char
import qualified Data.EnumSet as ES
import           Data.List
import           Data.Maybe
import           Graphics.UI.Gtk hiding(get, set)
import qualified Graphics.UI.Gtk as Gtk
import           System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import           System.Environment
import           System.FilePath (pathSeparator)
import           System.Glib.GError
import           System.Glib.UTFString
import           System.IO

--import ShiftGame.Helpers
import ShiftGame.GtkScenarioView
import ShiftGame.GtkShiftIO
import ShiftGame.Scenario
import ShiftGame.ScenarioController
import ShiftGame.ShiftIO

-- | Returns a string that displays the current scenario and the total number of scenarios.
getScenarioLevelDisplay :: ScenarioSettings sc -> String
getScenarioLevelDisplay scenSettings = (display . incId . currentScenario) scenSettings ++ "/" ++ (show . length . scenarioPool) scenSettings

-- | Creates a bar that helps the user navigate through several scenarios.
--   The passed @MVar@ is not accessed by this function but unpacked on each fired relevant event.
--   
--   The widget contains buttons to reset, decrement and increment the current scenario and a button to select another scenario input file.
createLevelSelector :: (ParsableScenario sc, ScenarioController ctrl sc IO) => ctrl                            -- ^ controller where to register listeners on
                                                                            -> MVar (GameSettings sc, ctrl)    -- ^ variable storing game state and controller for future events
                                                                            -> IO (HBox, ctrl)                 -- ^ created widget and updated controller,
                                                                                                               --   controller is not updated in the @MVar@, it should be done within the calling function
createLevelSelector ctrl gRef = do
   -- widget creation
   hbox <- hBoxNew False 4
   prevLevelBtn  <- buttonNewWithLabel "<"
   buttonSetFocusOnClick prevLevelBtn False
   nextLevelBtn  <- buttonNewWithLabel ">"
   buttonSetFocusOnClick nextLevelBtn False
   resetLevelBtn <- buttonNewWithLabel "r"
   buttonSetFocusOnClick resetLevelBtn False
   openLevelBtn  <- buttonNewWithLabel "o"
   buttonSetFocusOnClick openLevelBtn False

   displayLevelLbl <- labelNew (Just "0/0")

   let lblListener = LevelNewListener (\gRef' _ -> postGUIAsync $ do
          gameVar@(gameSettings, _) <- takeMVar gRef'
          labelSetText displayLevelLbl $ getScenarioLevelDisplay (gameSettings ^. lensScenarioSettings)
          putMVar gRef' gameVar) gRef
   ctrl' <- controllerAddListener ctrl lblListener

   -- packing
   boxPackStart hbox prevLevelBtn PackNatural 0
   boxPackStart hbox displayLevelLbl PackNatural 0
   boxPackStart hbox nextLevelBtn PackNatural 0
   boxPackEnd hbox resetLevelBtn PackNatural 0
   boxPackEnd hbox openLevelBtn PackNatural 0

   -- signals
   _ <- prevLevelBtn `on` buttonActivated $ void $ forkIO $ do 
            gameVar <- takeMVar gRef
            newScen <- setPrevScenarioLevel gameVar
            case newScen of
                 Just gameVar' -> putMVar gRef gameVar'
                 Nothing -> putMVar gRef gameVar
   _ <- nextLevelBtn `on` buttonActivated $ void $ forkIO $ do 
            gameVar <- takeMVar gRef
            newScen <- setNextScenarioLevel gameVar
            case newScen of
                 Just gameVar' -> putMVar gRef gameVar'
                 Nothing -> putMVar gRef gameVar
   _ <- resetLevelBtn `on` buttonActivated $ void $ forkIO $ do 
            gameVar <- takeMVar gRef
            gameVar' <- resetCurrentScenarioLevel gameVar
            putMVar gRef gameVar'
   _ <- openLevelBtn `on` buttonActivated $ void $ forkIO (void $ selectScenarioFile gRef)
   return (hbox, ctrl')

-- | Creates a standard shift window and uses the passed widget as playing field.
--   The passed @MVar@ is not accessed by this function but unpacked on each fired relevant event.
-- 
-- ==== See also
-- @'keyboardHandler'@
createShiftGameWindow :: (ParsableScenario sc, WidgetClass w, ScenarioController ctrl sc IO) => ctrl                          -- ^ controller where to register listeners on
                                                                                             -> EventM EKey Bool              -- ^ key event handler to register on passed playing field widget
                                                                                             -> MVar (GameSettings sc, ctrl)  -- ^ variable containing game state and controller for future events (see 'createLevelSelector')
                                                                                             -> w                             -- ^ playing field widget to add to the created window (among other generated widgets)
                                                                                             -> IO (Window, ctrl)             -- ^ created widget and updated controller,
                                                                                                                              --   controller is not updated in the @MVar@, it should be done within the calling function
createShiftGameWindow ctrl keyHandler gRef widget = do
   window <- windowNew
   vbox <- vBoxNew False 0    -- main container for window
   Gtk.set window [ containerChild := vbox]
   (levelBar, ctrl) <- createLevelSelector ctrl gRef
   boxPackStart vbox levelBar PackNatural 0
   boxPackStart vbox widget PackGrow 0
   -- widget key focus, key event
   widgetSetCanFocus widget True
   widgetGrabFocus widget
   -- add status bar
   (infobar, ctrl) <- createInfoBar ctrl
   boxPackEnd vbox infobar PackNatural 0
   -- add keyboard listener
   _ <- widget `on` keyPressEvent $ keyHandler

   -- finalize window
   widgetShowAll window
   return (window, ctrl)

-- | Loads a list of application icons (of possibly different size) from a standard path.
loadDefaultIconList :: IO [Pixbuf]
loadDefaultIconList = do
    let formats = fmap (('.':) . glibToString) pixbufGetFormats    -- supported image file extensions, prepended with '.'
        formats' = if ".jpeg" `elem` formats then ".jpg":formats else formats    -- jpg file ending is more common than jpeg
        iconDir = "data" ++ pathSeparator:"icon"
    dirExist <- doesDirectoryExist iconDir
    if dirExist
      then do content <- fmap (filter (\s -> isPrefixOf "app_icon_" s && any (`isSuffixOf` s) formats')) (getDirectoryContents iconDir)   -- filter appropriate files
              sequence $ fmap (\fp -> pixbufNewFromFile (iconDir ++ pathSeparator:fp)) content
      else putStrLn ("warning: failed to load icon directory " ++ iconDir) >> return []

-- | Modes for displayal of the game field.
data ViewMode = GraphicalViewMode -- ^ display within a window based on bitmap painting
              | TextualViewMode   -- ^ display within a window as ASCII characters
              deriving (Eq, Show, Read, Ord, Bounded, Enum)

-- | Interprets input arguments. Default 'ViewMode' is 'GraphicalViewMode'.
processInputParameters :: [String]    -- input parameters, result of 'getArgs' 
                       -> (Maybe String, ES.EnumSet ViewMode)    -- 'Just' the level path, if given, and an 'EnumMap' of selected display modes.
processInputParameters inputs =
  let hasAscii = isJust $ find ((== "-ascii") . map toLower) inputs
      hasFancy = isJust $ find ((== "-graphical") . map toLower) inputs
      viewModeList = (if hasAscii then TextualViewMode else GraphicalViewMode) : if hasFancy then [GraphicalViewMode] else []
      remInputs = [ a | a <- inputs, (not . (`elem` ["-ascii", "-graphical"])) a]
  in if null remInputs
      then (Nothing, ES.fromList viewModeList)
      else ((Just . head) remInputs, ES.fromList viewModeList)


main :: IO ()
main = do
   _ <- initGUI

   iconList <- catchGErrorJustDomain loadDefaultIconList (\(_::PixbufError) msg -> hPutStrLn stderr (glibToString msg) >> return [])
   windowSetDefaultIconList iconList

   -- read params
   args <- getArgs
   let (mbPath, windowModes) = processInputParameters args
   -- process level
       paramLevelPath = case mbPath of
                             Nothing -> "levels" ++ pathSeparator:"level.txt"
                             Just p -> p
   levelFileExist <- doesFileExist paramLevelPath
   mbLevelPath <- if levelFileExist
                    then return (Just paramLevelPath)
                    else do unless (isNothing mbPath) $
                                putStrLn ("failed to load level file: " ++ paramLevelPath ++ " does not exist")
                            showSelectScenarioDialog
   mbScenStates <- maybe (return Nothing) (readScenario) mbLevelPath
   -- initialize window
   let scenStates = case mbScenStates of
                         Nothing -> [emptyScenarioState]
                         Just [] -> [emptyScenarioState]
                         Just as -> as
       gameSett = initSettings scenStates
       sc = view lensScenarioSettings gameSett
       currentScen = getScenarioFromPool sc (currentScenario sc) :: ScenarioState MatrixScenario
       ctrl = initControllerState currentScen
   gRef <- newMVar (gameSett, ctrl)            :: IO (MVar (GameSettings MatrixScenario, ControllerState IO MatrixScenario))
   wRef <- newMVar []                          :: IO (MVar [Window])

   let keyHandler = keyboardHandler gRef wRef

   -- setup textual mode if it is selected
   (windows, ctrl) <- if (TextualViewMode `ES.member` windowModes)
                        then do (textArea, ctrl) <- createTextBasedView ctrl
                                (win, ctrl) <- createShiftGameWindow ctrl keyHandler gRef textArea
                                return ([win], ctrl)
                        else return ([], ctrl)
   -- setup graphical mode if it is selected
   (windows, ctrl) <- if (GraphicalViewMode `ES.member` windowModes)
                        then do (canvas, ctrl) <- createGraphicsBasedView ctrl currentScen
                                (win, ctrl) <- createShiftGameWindow ctrl keyHandler gRef canvas
                                return (win:windows, ctrl)
                        else return (windows, ctrl)
   -- empty window fallback if no mode is selected
   windows <- if (ES.null windowModes)
                then do win <- windowNew
                        widgetShowAll win
                        return (win:windows)
                else return windows
   -- close whole application when one window is closed
   sequence_ $ fmap (\win -> win `on` deleteEvent $ lift (quitAllWindows wRef) >> lift mainQuit >> return False) windows

   --(textArea, ctrl) <- createTextBasedView ctrl
   --(win1, ctrl) <- createShiftGameWindow ctrl keyHandler gRef textArea

   --(canvas, ctrl) <- createGraphicsBasedView ctrl currentScen
   --(win2, ctrl) <- createShiftGameWindow ctrl keyHandler gRef canvas
   

   --_ <- win1 `on` deleteEvent $ lift (quitAllWindows wRef) >> lift mainQuit >> return False
   --_ <- win2 `on` deleteEvent $ lift (quitAllWindows wRef) >> lift mainQuit >> return False
   (_, ctrl) <- setupAutoAdvanceLevel ctrl gRef

   _ <- swapMVar gRef (gameSett, ctrl)

   --wins <- takeMVar wRef
   --putMVar wRef (win1:win2:wins)
   storedWindows <- takeMVar wRef
   putMVar wRef (windows ++ storedWindows)
   
   mainGUI

-- | Creates a shift playing field that uses a text area to display the scenario.
--
-- ==== See also
-- @'createTextViewLink'@
createTextBasedView :: ScenarioController ctrl MatrixScenario IO => ctrl                   -- ^ input controller where to register as listener
                                                                 -> IO (TextView, ctrl)    -- ^ created @TextView@ and updated controller
createTextBasedView ctrl = do
    textArea <- textViewNew
    textViewSetEditable  textArea False
    textViewSetCursorVisible textArea False
    monoFnt <- fontDescriptionNew
    fontDescriptionSetFamily monoFnt "Monospace"
    widgetModifyFont textArea $ Just monoFnt -- set monospaced font
    textBuffer <- textViewGetBuffer textArea
    -- link with controller
    let lst = createTextViewLink textBuffer
    ctrl' <- controllerAddListener ctrl lst
    return (textArea, ctrl')

-- | Creates a shift playing field that draws the playing field using bitmaps.
--
-- ==== See also
-- @'createCanvasViewLink'@
createGraphicsBasedView :: (Scenario sc, ScenarioController ctrl sc IO) => ctrl                    -- ^ input controller where to register as listener
                                                                        -> ScenarioState sc        -- ^ initial scenario state
                                                                        -> IO (DrawingArea, ctrl)  -- ^ created @DrawingArea@ and updated controller
createGraphicsBasedView ctrl scs = do
    canvas <- drawingAreaNew
    widgetModifyBg canvas StateNormal (Color 0xFFFF 0xFFFF 0xFFFF)
    -- link with controller
    imgPool <- loadImagePool ("data" ++ pathSeparator:"img")
    lst <- createCanvasViewLink imgPool canvas scs
    ctrl' <- controllerAddListener ctrl lst
    return (canvas, ctrl')

-- | Creates a UI bar element that displays the amount of the player's moves and a victory notification.
--
-- ==== See also
-- @'createStatusBarLink'@
createInfoBar :: (Scenario sc, ScenarioController ctrl sc IO) => ctrl                    -- ^ input controller where to register as listener
                                                              -> IO (Statusbar, ctrl)    -- ^ created @Statusbar@ and updated controller
createInfoBar ctrl = do
    infobar <- statusbarNew
    lst <- createStatusBarLink infobar
    ctrl' <- controllerAddListener ctrl lst
    return (infobar, ctrl')

-- | Creates and registers a listener to advance to the next level automatically after some seconds.
--   The passed @MVar@ is not accessed by this function but unpacked on each fired relevant event.
--
-- ==== See also
-- @'createLevelProgressor'@
setupAutoAdvanceLevel :: (Scenario sc, ScenarioController ctrl sc IO) => ctrl                                -- ^ initial controller where to register as listener
                                                                      -> MVar (GameSettings sc, ctrl)        -- ^ variable storing the game settings and controller
                                                                      -> IO (LevelProgressor sc ctrl, ctrl)  -- ^ created listener and updated controller,
                                                                                                             --   does not update controller in @MVar@ which should be done by the calling function
setupAutoAdvanceLevel ctrl gRef = do
    let lst = createLevelProgressor gRef
    ctrl' <- controllerAddListener ctrl lst
    return (lst, ctrl')

-- | Creates initial 'GameSettings'.
initSettings :: Scenario sc => [ScenarioState sc] -> GameSettings sc
initSettings s = let 
    uic = UserInputControl { keysLeft  = map (keyFromName . stringToGlib) ["Left", "a", "A"]
                           , keysRight = map (keyFromName . stringToGlib) ["Right", "d", "D"]
                           , keysUp    = map (keyFromName . stringToGlib) ["Up", "w", "W"]
                           , keysDown  = map (keyFromName . stringToGlib) ["Down", "s", "S"]
                           , keysQuit  = map (keyFromName . stringToGlib) ["Escape"]
                           , keysUndo  = map (keyFromName . stringToGlib) ["minus", "KP_Subtract"]
                           , keysRedo  = map (keyFromName . stringToGlib) ["plus", "KP_Add"]
                           , keysReset = map (keyFromName . stringToGlib) ["r", "R"]
                           , keysNext  = map (keyFromName . stringToGlib) ["n", "N"]
                           , keysPrev  = map (keyFromName . stringToGlib) ["p", "P"]
                           , keysLoad  = map (keyFromName . stringToGlib) ["o", "O"]
                           , movementMode = MovementEnabled
                           }
    sc = ScenarioSettings { scenarioPool    = s
                          , currentScenario = 0
                          }
  in GameSettings sc uic NoChangeStalled


-- | Destroys all given windows and clears the @MVar@.
quitAllWindows :: MVar [Window] -> IO ()
quitAllWindows wRef = do
    windows <- takeMVar wRef
    mapM_ widgetDestroy windows
    putMVar wRef []


-- implementation detail: GTK event handling does not (easily) allow mixing the Event monad with e. g. State or Reader
-- that is the reason why an 'MVar' is used.
-- | Processes keyboard events and determines the resulting player action.
--   Takes @MVar (GameSettings sc, ctrl)@ before @MVar [Window]@.
keyboardHandler :: (ParsableScenario sc, ScenarioController ctrl sc IO) => MVar (GameSettings sc, ctrl)    -- ^ variable storing game state and controller
                                                                        -> MVar [Window]                   -- ^ windows to close when quitting the application
                                                                        -> EventM EKey Bool                -- ^ key event to register on other widgets
keyboardHandler gRef wRef = do 
    (g@(GameSettings scenSettings uic stalled), ctrl) <- (lift . readMVar) gRef    -- only readMVar
    keyV <- eventKeyVal
    -- test to quit game
    b <- if (keyV `elem` keysQuit uic)
      then lift (quitAllWindows wRef) >> lift mainQuit >> return True
      -- reset current level
      else if (keyV `elem` keysReset uic)
      then lift $ forkIO (do
             gameVar <- takeMVar gRef
             gameVar' <- resetCurrentScenarioLevel gameVar
             putMVar gRef gameVar'
           ) >> return True
      -- set next level in pool
      else if (keyV `elem` keysNext uic)
      then lift $ forkIO (do
             gameVar <- takeMVar gRef
             newScen <- setNextScenarioLevel gameVar
             case newScen of
                  Just gameVar' -> putMVar gRef gameVar'
                  Nothing -> putMVar gRef gameVar
           ) >> return True
      -- set previous level in pool
      else if (keyV `elem` keysPrev uic)
      then lift $ forkIO (do
             gameVar <- takeMVar gRef
             newScen <- setPrevScenarioLevel gameVar
             case newScen of
                  Just gameVar' -> putMVar gRef gameVar'
                  Nothing -> putMVar gRef gameVar
           ) >> return True
      -- undo last move
      else if (keyV `elem` keysUndo uic)
      then lift $ forkIO (do
             (gSett, ctrl) <- takeMVar gRef
             (err, ctrl') <- runStateT undoAction ctrl
             gSett' <- case err of
                            Just e  -> do unless (e == NoAction) $ putStrLn ("undo : " ++ show e)
                                          return gSett
                            Nothing -> return $ gSett & (lensUserInputControl . lensMovementMode .~ MovementEnabled)
                                                      . (lensStalledScenarioChange .~ NoChangeStalled)
             putMVar gRef (gSett', ctrl')
           ) >> return True
      -- redo last undo
      else if (keyV `elem` keysRedo uic)
      then lift $ forkIO (do
             (gSett, ctrl) <- takeMVar gRef
             (err, ctrl') <- runStateT redoAction ctrl
             gSett' <- case err of
                            Just e -> do unless (e == NoAction) $ putStrLn ("redo : " ++ show e)
                                         return gSett
                            Nothing -> return $ gSett & (lensUserInputControl . lensMovementMode .~ MovementEnabled)
                                                      . (lensStalledScenarioChange .~ NoChangeStalled)
             putMVar gRef (gSett', ctrl')
           ) >> return True
      -- open level file selection dialog
      else if (keyV `elem` keysLoad uic)
      then lift $ forkIO (void $ selectScenarioFile gRef) >> return True 
      -- player movement
      else do
          let mbPlayerAction = if keyV `elem` keysLeft uic  then Just MLeft
                          else if keyV `elem` keysRight uic then Just MRight
                          else if keyV `elem` keysUp uic    then Just MUp
                          else if keyV `elem` keysDown uic  then Just MDown
                          else Nothing
          -- test if valid movement key has been pressed
          case mbPlayerAction of
               Nothing -> -- do lift . putStrLn $ "unknown key command: (" ++ show keyV ++ ") " ++ (show . keyName) keyV   -- debug message
                             return False
               Just action -> do
                 gameVar@(GameSettings scenSettings uic _, ctrl) <- lift $ takeMVar gRef    -- get lock
                 -- test if player movement is enabled
                 if (view (lensMovementMode) uic == MovementEnabled)
                   then lift $ void $ forkIO (do
                          gameVar@(_, ctrl) <- takeMVar gRef
                          -- (putStrLn . show) action    -- debug message
                          (denyReason, ctrl') <- runStateT (runPlayerMove action) ctrl
                          -- when (isJust denyReason) $
                          --     (putStrLn . show . fromJust) denyReason    -- failure debug message
                          putMVar gRef (gameVar & _2 .~ ctrl')
                        )
                   -- if movement is disabled AND delayed level change is stalled: perform now
                   else lift $ maybe (return ())
                                     (\_ -> void $ forkIO $ do
                                         gameVar@(g@(GameSettings scenSettings uic stalled), ctrl) <- takeMVar gRef
                                         let mbScenId = stalled ^? lensStalledScenarioId
                                         gameVar' <- maybe (return gameVar) (\scenId -> do
                                              let mbNextScen = setCurrentScenario scenSettings scenId
                                              maybe (return gameVar)    -- something is wrong with stalled change, do nothing
                                                    (\(scenSettings', newScen) -> do    -- perform stalled change
                                                        (_, ctrl') <- runStateT (setScenario newScen) ctrl
                                                        return (g & (lensUserInputControl . lensMovementMode .~ MovementEnabled)
                                                                  . (lensScenarioSettings .~ scenSettings')
                                                                  . (lensStalledScenarioChange .~ NoChangeStalled)
                                                                  , ctrl')
                                                    ) mbNextScen) mbScenId
                                         putMVar gRef gameVar'
                                     ) (gameVar ^? _1 . lensStalledScenarioChange . _ChangeStalled)
                 lift $ putMVar gRef gameVar
                 return True
    return b

