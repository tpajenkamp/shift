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
                         putStrLn $ "player: " ++ (show . playerCoord) myScenarioState
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
   textArea <- textViewNew
   textViewSetEditable  textArea False
   textViewSetCursorVisible textArea False
   monoFnt <- fontDescriptionNew
   fontDescriptionSetFamily monoFnt "Monospace"
   widgetModifyFont textArea $ Just monoFnt -- set monospaced font
   textBuffer <- textViewGetBuffer textArea
   textBufferSetByteString textBuffer (B.pack "Hallo Welt!")
   -- widget key focus, key event, link with controller
   widgetSetCanFocus textArea True
   let lst = createTextViewLink textBuffer :: UpdateListener IO MatrixScenario
   ctrl <- initController scenState lst
   settingsRef <- newIORef (initSettings, ctrl)
   _ <- textArea `on` keyPressEvent $ keyboardHandler settingsRef 
   -- finalize window
   set window [ containerChild := textArea]
   _ <- window `on` deleteEvent $ lift mainQuit >> return False
   widgetShowAll window
   mainGUI

initSettings :: ControlSettings
initSettings = ControlSettings { keysLeft  = map (keyFromName . stringToGlib) ["Left", "a", "A"]   -- Left arrow key
                               , keysRight = map (keyFromName . stringToGlib) ["Right", "d", "D"]  -- Right arrow key
                               , keysUp    = map (keyFromName . stringToGlib) ["Up", "w", "W"]     -- Up arrow key
                               , keysDown  = map (keyFromName . stringToGlib) ["Down", "s", "S"]   -- Down arrow key
                               }

initController :: (Functor m, Scenario sc, ScenarioController ctrl sc m) => ScenarioState sc -> UpdateListener m sc -> m ctrl
initController sc lst = fmap snd $ runStateT (addListenerM lst) (initControllerState sc)
