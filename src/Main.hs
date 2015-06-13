module Main where

--import           Control.DeepSeq
import           Control.Exception.Base
import           Control.Monad.State.Lazy
--import           Control.Monad.Trans(liftIO)
import           Data.Attoparsec.ByteString.Char8 (parse, parseOnly)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Either
import           Helpers
import           System.Environment

import Graphics.UI.Gtk
import Scenario
import ScenarioController
import ScenarioParser

testParser :: ByteString -> IO ()
testParser levelRaw = do let possiblyParsed = parseOnly (runStateT parseScenario initParseState) levelRaw
                         unless (isRight possiblyParsed) $
                             do guard False
                                (error . fromLeft) possiblyParsed
                         let (myScenarioState, myParseState) = fromRight possiblyParsed
                         _ <- evaluate myScenarioState
                         putStrLn "warnings:"
                         putStrLn $ (unlines . map show . warnings) myParseState
                         (putStrLn . showScenario . scenario) myScenarioState


main :: IO ()
main = do
   args <- getArgs
   let levelPath = if null args
                     then "level.txt"
                     else head args
   levelRaw <- B.readFile levelPath
   testParser levelRaw
   --
   _ <- initGUI
   window <- windowNew
   textArea <- textViewNew
   textViewSetEditable  textArea False
   textViewSetCursorVisible textArea False
   textBuffer <- textViewGetBuffer textArea
   textBufferSetByteString textBuffer (B.pack "Hallo Welt!")
   set window [ containerChild := textArea]
   _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
   widgetShowAll window
   mainGUI
