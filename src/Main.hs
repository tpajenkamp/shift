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
import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Attoparsec.ByteString.Char8 (parse, parseOnly)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Either
import           Data.IORef
import           Data.Maybe
import           Graphics.UI.Gtk
import           System.Environment
import           System.Glib.UTFString


import ShiftGame.Helpers
import ShiftGame.GTKScenarioView
import ShiftGame.Scenario
import ShiftGame.ScenarioController
import ShiftGame.ScenarioParser

testParser :: ByteString -> IO (ScenarioState MatrixScenario)
testParser levelRaw = do let possiblyParsed = parseOnly (runStateT parseScenario initParseState) levelRaw
                         unless (isRight possiblyParsed) $
                             do guard False
                                (error . fromLeft) possiblyParsed
                         let (myScenarioState, myParseState) = fromRight possiblyParsed
                         _ <- evaluate myScenarioState
                         putStrLn "warnings:"
                         putStrLn $ (unlines . map show . warnings) myParseState
                         putStrLn $ "player: " ++ (show . playerCoord) myScenarioState ++ " empty targets: " ++ (show . emptyTargets) myScenarioState
                         (B.putStrLn . flip showScenarioWithPlayer (playerCoord myScenarioState) . scenario) myScenarioState
                         return myScenarioState


main :: IO ()
main = do
   -- read level
   args <- getArgs
   let levelPath = if null args
                     then "level.txt"
                     else head args
   levelRaw <- B.readFile levelPath
   scenState <- testParser levelRaw :: IO (ScenarioState MatrixScenario)
   -- initialize window
   _ <- initGUI
   window <- windowNew
   -- add text view
   (textArea, ctrl) <- createTextBasedView (initControllerState scenState :: ControllerState IO MatrixScenario)
   -- widget key focus, key event
   widgetSetCanFocus textArea True
   settingsRef <- newIORef (initSettings scenState, ctrl)
   _ <- textArea `on` keyPressEvent $ keyboardHandler settingsRef 
   -- finalize window
   set window [ containerChild := textArea]
   _ <- window `on` deleteEvent $ lift mainQuit >> return False
   widgetShowAll window

   window2 <- windowNew
   (textArea2, ctrl2) <- createTextBasedView ctrl
   widgetSetCanFocus textArea2 True
   modifyIORef settingsRef (\(s, c) -> (s, ctrl2))
   _ <- textArea2 `on` keyPressEvent $ keyboardHandler settingsRef 
   set window2 [ containerChild := textArea2]
   widgetShowAll window2
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

initSettings :: Scenario sc => ScenarioState sc -> ControlSettings sc
initSettings s = ControlSettings { keysLeft  = map (keyFromName . stringToGlib) ["Left", "a", "A"]
                                 , keysRight = map (keyFromName . stringToGlib) ["Right", "d", "D"]
                                 , keysUp    = map (keyFromName . stringToGlib) ["Up", "w", "W"]
                                 , keysDown  = map (keyFromName . stringToGlib) ["Down", "s", "S"]
                                 , keysQuit  = map (keyFromName . stringToGlib) ["Escape"]
                                 , keysUndo  = map (keyFromName . stringToGlib) ["minus", "KP_Subtract"]
                                 , keysRedo  = map (keyFromName . stringToGlib) ["plus", "KP_Add"]
                                 , keysReset = map (keyFromName . stringToGlib) ["r", "R"]
                                 , initialScenario = s
                                 }
