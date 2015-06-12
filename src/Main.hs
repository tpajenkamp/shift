module Main where

import           Control.DeepSeq
import           Control.Exception.Base
import           Control.Monad.State.Lazy
import           Control.Monad.Trans(liftIO)
import           Data.Attoparsec.ByteString.Char8 (parse, parseOnly)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Either
import           Helpers
import           System.Environment

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Multiline.TextView
import Scenario
import ScenarioParser

testParser :: ByteString -> IO ()
testParser levelRaw = do let possiblyParsed = parseOnly (runStateT parseScenario initParseState) levelRaw
                         unless (isRight possiblyParsed) $
                             do guard False
                                (error . fromLeft) possiblyParsed
                         let (scenarioState, parseState) = fromRight possiblyParsed
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
   textArea <- textViewNew
   textViewSetEditable  textArea False
   textViewSetCursorVisible textArea False
   textBuffer <- textViewGetBuffer textArea
   textBufferSetByteString textBuffer (B.pack "Hallo Welt!")
   set window [ containerChild := textArea]
   window `on` deleteEvent $ liftIO mainQuit >> return False
   widgetShowAll window
   mainGUI
