{-# LANGUAGE FlexibleContexts #-}
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

--import           Control.DeepSeq
import           Control.Concurrent
--import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Maybe
import           Graphics.UI.Gtk hiding(get, set)
import qualified Graphics.UI.Gtk as Gtk
import           System.Directory (doesFileExist)
import           System.Environment
import           System.FilePath (pathSeparator)
import           System.Glib.UTFString

import ShiftGame.Helpers
import ShiftGame.GtkScenarioView
import ShiftGame.GtkShiftIO
import ShiftGame.Scenario
import ShiftGame.ScenarioController
import ShiftGame.ShiftIO


createLevelSelector :: ScenarioController ctrl MatrixScenario IO => MVar (UserInputControl) -> MVar (ScenarioSettings MatrixScenario, ctrl) -> IO HBox
createLevelSelector uRef sRef = do
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
   displayLevelLbl <- labelNew (Just "0/xxx")

   -- packing
   boxPackStart hbox prevLevelBtn PackNatural 0
   boxPackStart hbox displayLevelLbl PackNatural 0
   boxPackStart hbox nextLevelBtn PackNatural 0
   boxPackEnd hbox resetLevelBtn PackNatural 0
   boxPackEnd hbox openLevelBtn PackNatural 0

   -- signals
   _ <- prevLevelBtn `on` buttonActivated $ do
            scenVar@(scenSettings, ctrl) <- takeMVar sRef
            uic <- takeMVar uRef
            newScen <- setPrevScenarioLevel uic scenVar
            case newScen of
                 Just (uic', scenVar') -> putMVar uRef uic' >> putMVar sRef scenVar'
                 Nothing -> putMVar uRef uic >> putMVar sRef scenVar
   _ <- nextLevelBtn `on` buttonActivated $ do
            scenVar@(scenSettings, ctrl) <- takeMVar sRef
            uic <- takeMVar uRef
            newScen <- setNextScenarioLevel uic scenVar
            case newScen of
                 Just (uic', scenVar') -> putMVar uRef uic' >> putMVar sRef scenVar'
                 Nothing -> putMVar uRef uic >> putMVar sRef scenVar
   _ <- resetLevelBtn `on` buttonActivated $ (putStrLn "want reset level")
   _ <- openLevelBtn `on` buttonActivated $ void $ forkIO (void $ selectScenarioFile uRef sRef)
   return hbox

createShiftGameWindow :: (WidgetClass w, ScenarioController ctrl MatrixScenario IO) => ctrl -> EventM EKey Bool -> MVar (UserInputControl) -> MVar (ScenarioSettings MatrixScenario, ctrl) -> w -> IO (Window, ctrl)
createShiftGameWindow ctrl keyHandler uRef sRef widget = do
   window <- windowNew
   vbox <- vBoxNew False 0    -- main container for window
   Gtk.set window [ containerChild := vbox]
   levelBar <- createLevelSelector uRef sRef
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


main :: IO ()
main = do
   _ <- initGUI

   -- read level
   args <- getArgs
   let paramLevelPath = if null args
                          then "level.txt"
                          else head args
   levelFileExist <- doesFileExist paramLevelPath
   mbLevelPath <- if levelFileExist
                    then return (Just paramLevelPath)
                    else do unless (null args) $
                                putStrLn ("failed to load level file: " ++ paramLevelPath ++ "does not exist")
                            showSelectScenarioDialog
   mbScenStates <- maybe (return Nothing) (readScenario) mbLevelPath
   -- initialize window
   let scenStates = case mbScenStates of
                         Nothing -> [emptyScenarioState]
                         Just [] -> [emptyScenarioState]
                         Just as -> as
       (uc, sc) = initSettings scenStates
       currentScen = getScenarioFromPool sc (currentScenario sc) :: ScenarioState MatrixScenario
       ctrl = initControllerState currentScen :: ControllerState IO MatrixScenario
   uRef <- newMVar uc            :: IO (MVar UserInputControl)
   sRef <- newMVar (sc, ctrl)    :: IO (MVar (ScenarioSettings MatrixScenario, ControllerState IO MatrixScenario))
   wRef <- newMVar []            :: IO (MVar [Window])

   let keyHandler = keyboardHandler uRef sRef wRef

   (textArea, ctrl) <- createTextBasedView ctrl
   (win1, ctrl) <- createShiftGameWindow ctrl keyHandler uRef sRef textArea

   (canvas, ctrl) <- createGraphicsBasedView ctrl currentScen
   (win2, ctrl) <- createShiftGameWindow ctrl keyHandler uRef sRef canvas
   
   _ <- swapMVar sRef (sc, ctrl)

   _ <- win1 `on` deleteEvent $ lift (quitAllWindows wRef) >> lift mainQuit >> return False
   _ <- win2 `on` deleteEvent $ lift (quitAllWindows wRef) >> lift mainQuit >> return False
   (_, ctrl) <- autoAdvanceLevel uRef sRef

   wins <- takeMVar wRef
   putMVar wRef (win1:win2:wins)
   
   mainGUI


createTextBasedView :: ScenarioController ctrl MatrixScenario IO => ctrl -> IO (TextView, ctrl)
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


createGraphicsBasedView :: ScenarioController ctrl MatrixScenario IO => ctrl -> ScenarioState MatrixScenario -> IO (DrawingArea, ctrl)
createGraphicsBasedView ctrl scs = do
    canvas <- drawingAreaNew
    widgetModifyBg canvas StateNormal (Color 0xFFFF 0xFFFF 0xFFFF)
    -- link with controller
    imgPool <- loadImagePool ("data" ++ pathSeparator:"img")
    lst <- createCanvasViewLink imgPool canvas scs
    ctrl' <- controllerAddListener ctrl lst
    return (canvas, ctrl')

createInfoBar :: (Scenario sc, ScenarioController ctrl sc IO) => ctrl -> IO (Statusbar, ctrl)
createInfoBar ctrl = do
    infobar <- statusbarNew
    lst <- createStatusBarLink infobar
    ctrl' <- controllerAddListener ctrl lst
    return (infobar, ctrl')

autoAdvanceLevel :: (Scenario sc, ScenarioController ctrl sc IO) => MVar UserInputControl -> MVar (ScenarioSettings sc, ctrl) -> IO (LevelProgressor sc ctrl, ctrl)
autoAdvanceLevel uRef sRef = do
    (scenSettings, ctrl) <- takeMVar sRef
    let lst = LevelProgressor uRef sRef
    ctrl' <- controllerAddListener ctrl lst
    putMVar sRef (scenSettings, ctrl')
    return (lst, ctrl')

initSettings :: Scenario sc => [ScenarioState sc] -> (UserInputControl, ScenarioSettings sc)
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
                           , inputMode = InputMode MovementEnabled NoChangeStalled
                           }
    sc = ScenarioSettings { scenarioPool    = s
                          , currentScenario = 0
                          }
  in (uic, sc)



-- implementation detail: GTK event handling does not (easily) allow mixing the Event monad with e. g. State or Reader
-- that is the reason why an 'MVar' is used.
-- | Processes keyboard events and determines the resulting player action.
--   Takes @MVar (ScenarioSettings sc, ctrl)@ before @MVar UserInputControl@ before @MVar [Window]@.
keyboardHandler :: (ScenarioController ctrl MatrixScenario IO) => MVar (UserInputControl) -> MVar (ScenarioSettings MatrixScenario, ctrl) -> MVar [Window] -> EventM EKey Bool
keyboardHandler uRef sRef wRef = do 
    keySettings <- (lift . readMVar) uRef
    keyV <- eventKeyVal
    -- test to quit game
    b <- if (keyV `elem` keysQuit keySettings)
      then lift (quitAllWindows wRef) >> lift mainQuit >> return True
      -- reset current level
      else if (keyV `elem` keysReset keySettings)
      then lift $ forkIO (do
             (scenSettings, ctrl) <- takeMVar sRef
             keySettings <- takeMVar uRef
             let currentScen = getScenarioFromPool scenSettings (currentScenario scenSettings)
             (_, ctrl') <- runStateT (setScenario currentScen) ctrl
             -- there may be a delayed level change -> enable player movement
             putMVar uRef (keySettings & lensInputMode %~ (lensMovementMode .~ MovementEnabled) . (lensScenarioChangeMode .~ NoChangeStalled))
             putMVar sRef (scenSettings, ctrl')
           ) >> return True
      -- set next level in pool
      else if (keyV `elem` keysNext keySettings)
      then lift $ forkIO (do
             var@(scenSettings, ctrl) <- takeMVar sRef
             keySettings <- takeMVar uRef
             newScen <- setNextScenarioLevel keySettings var
             case newScen of
                  Just (keySettings', var') -> putMVar uRef keySettings' >> putMVar sRef var'
                  Nothing -> putMVar uRef keySettings >> putMVar sRef var
           ) >> return True
      -- set previous level in pool
      else if (keyV `elem` keysPrev keySettings)
      then lift $ forkIO (do
             var@(scenSettings, ctrl) <- takeMVar sRef
             keySettings <- takeMVar uRef
             newScen <- setPrevScenarioLevel keySettings var
             case newScen of
                  Just (keySettings', var') -> putMVar uRef keySettings' >> putMVar sRef var'
                  Nothing -> putMVar uRef keySettings >> putMVar sRef var
           ) >> return True
      -- undo last move
      else if (keyV `elem` keysUndo keySettings)
      then lift $ forkIO (do
             (scenSettings, ctrl) <- takeMVar sRef
             keySettings <- takeMVar uRef
             (err, ctrl') <- runStateT undoAction ctrl
             case err of
                  Just e -> do putMVar uRef keySettings
                               putStrLn ("undo : " ++ show e)
                  Nothing -> do putMVar uRef (keySettings & lensInputMode %~
                                    (lensMovementMode .~ MovementEnabled) . (lensScenarioChangeMode .~ NoChangeStalled))
             putMVar sRef (scenSettings, ctrl')
           ) >> return True
      -- redo last undo
      else if (keyV `elem` keysRedo keySettings)
      then lift $ forkIO (do
             (scenSettings, ctrl) <- takeMVar sRef
             keySettings <- takeMVar uRef
             (err, ctrl') <- runStateT redoAction ctrl
             case err of
                  Just e -> do putMVar uRef keySettings
                               putStrLn ("redo : " ++ show e)
                  Nothing -> do putMVar uRef (keySettings & lensInputMode %~
                                    (lensMovementMode .~ MovementEnabled) . (lensScenarioChangeMode .~ NoChangeStalled))
             putMVar sRef (scenSettings, ctrl')
           ) >> return True
      -- open level file selection dialog
      else if (keyV `elem` keysLoad keySettings)
      then lift $ forkIO (void $ selectScenarioFile uRef sRef) >> return True 
      -- player movement
      else do
          let mbPlayerAction = if keyV `elem` keysLeft keySettings  then Just MLeft
                          else if keyV `elem` keysRight keySettings then Just MRight
                          else if keyV `elem` keysUp keySettings    then Just MUp
                          else if keyV `elem` keysDown keySettings  then Just MDown
                          else Nothing
          -- test if valid movement key has been pressed
          case mbPlayerAction of
               Nothing -> do lift . putStrLn $ "unknown key command: (" ++ show keyV ++ ") " ++ (show . keyName) keyV
                             return False
               Just action -> do
                 var@(scenSettings, ctrl) <- lift $ takeMVar sRef
                 keySettings <- lift $ takeMVar uRef
                 -- test if player movement is enabled
                 if (view (lensInputMode . lensMovementMode) keySettings == MovementEnabled)
                   then lift $ forkIO (do
                          (scenSettings, ctrl) <- takeMVar sRef
                          (putStrLn . show) action
                          (denyReason, ctrl') <- runStateT (runPlayerMove action) ctrl
                          when (isJust denyReason) $
                              (putStrLn . show . fromJust) denyReason    -- failure
                          putMVar sRef (scenSettings, ctrl')
                        ) >> putMVar uRef keySettings >> putMVar sRef var
                   -- if movement is disabled AND delayed level change is stalled: perform now
                   else lift $ maybe (do putMVar uRef keySettings >> putMVar sRef var)
                                     (\(_, scId) -> do
                                       let mbNextScen = setCurrentScenario scenSettings scId
                                       maybe (putMVar uRef keySettings >> putMVar sRef var)    -- something is wrong with stalled change, do nothing
                                             (\(scenSettings', newScen) -> do
                                                 (_, ctrl') <- runStateT (setScenario newScen) ctrl
                                                 putMVar uRef (keySettings & lensInputMode %~ (lensMovementMode .~ MovementEnabled)
                                                                                            . (lensScenarioChangeMode .~ NoChangeStalled))
                                                 putMVar sRef (scenSettings', ctrl'))          -- perform stalled change
                                             mbNextScen
                                     ) (keySettings ^? lensInputMode . lensScenarioChangeMode . _ChangeStalled)
                 return True
    return b

