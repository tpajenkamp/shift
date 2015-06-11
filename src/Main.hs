module Main where

import           Control.DeepSeq
import           Control.Exception.Base
import           Control.Monad.State.Lazy
import           Control.Monad.Trans(liftIO)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           System.Environment

import Graphics.UI.Gtk
import Scenario
import ScenarioParser

testParser :: ByteString -> IO ()
testParser levelRaw = do let (scenarioState, parseState) = runState (parseScenario levelRaw) initParseState
                         evaluate scenarioState
                         putStrLn "warnings:"
                         putStrLn $ (unlines . map show . warnings) parseState
                         (putStrLn . showScenario . scenario) scenarioState


main = do
   args <- getArgs
   let levelPath = if null args
                     then "level.txt"
                     else head args
   levelRaw <- B.readFile levelPath
   testParser levelRaw
   --
   initGUI
   window <- windowNew
   window `on` deleteEvent $ liftIO mainQuit >> return False
   widgetShowAll window
   mainGUI
