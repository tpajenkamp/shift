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
import           System.Environment
import           System.FilePath (pathSeparator)
import           System.Glib.UTFString

import ShiftGame.Helpers
import ShiftGame.GtkScenarioView
import ShiftGame.GtkShiftIO
import ShiftGame.Scenario
import ShiftGame.ScenarioController
import ShiftGame.ShiftIO


createShiftGameWindow :: (WidgetClass w, ScenarioController ctrl MatrixScenario IO) => ctrl -> EventM EKey Bool -> w -> IO (Window, ctrl)
createShiftGameWindow ctrl keyHandler widget = do
   window <- windowNew
   vbox <- vBoxNew False 0    -- main container for window
   Gtk.set window [ containerChild := vbox]
   boxPackStart vbox widget PackGrow 0
   -- widget key focus, key event
   widgetSetCanFocus widget True
   -- add status bar
   (infobar, ctrl) <- createInfoBar ctrl
   boxPackStart vbox infobar PackRepel 0
   -- add keyboard listener
   _ <- widget `on` keyPressEvent $ keyHandler

   -- finalize window
   widgetShowAll window
   return (window, ctrl)


-- todo: level choosing if input file is no valid level file

main :: IO ()
main = do
   _ <- initGUI

   -- read level
   args <- getArgs
   levelPath <- if null args
                  then do mbPath <- showSelectScenarioDialog :: IO (Maybe FilePath)
                          return $ maybe "level.txt" id mbPath
                  else return $ head args
   mbScenStates <- readScenario levelPath
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
   (win1, ctrl) <- createShiftGameWindow ctrl keyHandler textArea

   (canvas, ctrl) <- createGraphicsBasedView ctrl currentScen
   (win2, ctrl) <- createShiftGameWindow ctrl keyHandler canvas
   
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
             putStrLn "level reset"
             -- there may be a delayed level change -> disable and enable player movement
             putMVar uRef (keySettings & lensInputMode %~ (lensMovementMode .~ MovementEnabled) . (lensScenarioChangeMode .~ NoChangeStalled))
             putMVar sRef (scenSettings, ctrl')
           ) >> return True
      -- set next level in pool
      else if (keyV `elem` keysNext keySettings)
      then lift $ forkIO (do
             var@(scenSettings, ctrl) <- takeMVar sRef
             keySettings <- takeMVar uRef
             new <- case increaseScenarioId scenSettings of
                         Just (scenSettings', nextScen, _) -> do
                              (_, ctrl') <- runStateT (setScenario nextScen) ctrl
                              -- there may be a delayed level change -> disable and enable player movement
                              putMVar uRef (keySettings & lensInputMode %~ (lensMovementMode .~ MovementEnabled) . (lensScenarioChangeMode .~ NoChangeStalled))
                              putStrLn "next level"
                              return (scenSettings', ctrl')
                         Nothing -> putMVar uRef keySettings >> return var
             putMVar sRef new
           ) >> return True
      -- set previous level in pool
      else if (keyV `elem` keysPrev keySettings)
      then lift $ forkIO (do
             var@(scenSettings, ctrl) <- takeMVar sRef
             keySettings <- takeMVar uRef
             new <- case decreaseScenarioId scenSettings of
                         Just (scenSettings', prevScen, _) -> do
                              (_, ctrl') <- runStateT (setScenario prevScen) ctrl
                              -- there may be a delayed level change -> disable and enable player movement
                              putMVar uRef (keySettings & lensInputMode %~ (lensMovementMode .~ MovementEnabled) . (lensScenarioChangeMode .~ NoChangeStalled))
                              putStrLn "next level"
                              return (scenSettings', ctrl')
                         Nothing -> putMVar uRef keySettings >> return var
             putMVar sRef new
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
                                putStrLn "undo"
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
                                putStrLn "redo"
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

