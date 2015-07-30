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
import           Control.Concurrent.MVar
--import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Either
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


createTextViewWindow :: (ScenarioController ctrl MatrixScenario IO) => MVar (ScenarioSettings MatrixScenario, ctrl) -> EventM EKey Bool -> IO (Window, ctrl)
createTextViewWindow sRef keyHandler = do
   (scenSettings, ctrl) <- takeMVar sRef
   
   window <- windowNew
   vbox <- vBoxNew False 0    -- main container for window
   Gtk.set window [ containerChild := vbox]
   -- add text view
   (textArea, ctrl) <- createTextBasedView ctrl
   boxPackStart vbox textArea PackGrow 0
   -- widget key focus, key event
   widgetSetCanFocus textArea True
   -- add status bar
   (infobar, ctrl) <- createInfoBar ctrl
   boxPackStart vbox infobar PackRepel 0
   -- add keyboard listener
   putMVar sRef (scenSettings, ctrl)
   _ <- textArea `on` keyPressEvent $ keyHandler

   -- finalize window
   widgetShowAll window
   return (window, ctrl)

createGraphicsViewWindow :: (ScenarioController ctrl MatrixScenario IO) => MVar (ScenarioSettings MatrixScenario, ctrl) -> EventM EKey Bool -> IO (Window, ctrl)
createGraphicsViewWindow sRef keyHandler = do
   (scenSettings, ctrl) <- takeMVar sRef
   let scenState = getScenarioFromPool scenSettings (currentScenario scenSettings)

   window <- windowNew
   vbox2 <- vBoxNew False 0    -- main container for window2
   Gtk.set window [ containerChild := vbox2]
   -- add graphical view
   (canvas, ctrl) <- createGraphicsBasedView ctrl scenState
   widgetSetCanFocus canvas True
   boxPackStart vbox2 canvas PackGrow 0
   -- add status bar
   (infobar2, ctrl) <- createInfoBar ctrl
   boxPackStart vbox2 infobar2 PackRepel 0
   -- add keyboard listener
   putMVar sRef (scenSettings, ctrl)
   _ <- canvas `on` keyPressEvent $ keyHandler

   -- finalize window
   widgetShowAll window
   return (window, ctrl)


main :: IO ()
main = do
   _ <- initGUI

   -- read level
   args <- getArgs
   levelPath <- if null args
                  then do mbPath <- showSelectScenarioDialog :: IO (Maybe FilePath)
                          return $ maybe "level.txt" id mbPath
                  else return $ head args
   scenStates <- readScenario levelPath
   -- initialize window
   let scenState = case scenStates of [] -> emptyScenarioState; a:_ -> a
       ctrl = initControllerState scenState :: ControllerState IO MatrixScenario
       (uc, sc) = initSettings scenStates
   uRef <- newMVar uc            :: IO (MVar UserInputControl)
   sRef <- newMVar (sc, ctrl)    :: IO (MVar (ScenarioSettings MatrixScenario, ControllerState IO MatrixScenario))
   wRef <- newMVar []            :: IO (MVar [Window])

   let keyHandler = keyboardHandler uRef sRef wRef

   (win1, ctrl) <- createTextViewWindow sRef keyHandler
   (win2, ctrl) <- createGraphicsViewWindow sRef keyHandler

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
                           , inputMode = InputMode MovementEnabled NoChangeStalled
                           }
    sc = ScenarioSettings { scenarioPool    = s
                          , currentScenario = 0
                          }
  in (uic, sc)
