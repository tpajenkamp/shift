module Main where

--import           Control.DeepSeq
import           Control.Exception.Base
import           Control.Monad.State.Lazy
--import           Control.Monad.Trans(liftIO)
import           Data.Attoparsec.ByteString.Char8 (parse, parseOnly)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Either
import           Data.Maybe
import           Helpers
import           System.Environment

import Graphics.UI.Gtk

import GTKScenarioView
import Scenario
import ScenarioController
import ScenarioParser

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
                         (B.putStrLn . showScenario . scenario) myScenarioState
                         return myScenarioState


main :: IO ()
main = do
   -- read level
   args <- getArgs
   let levelPath = if null args
                     then "level.txt"
                     else head args
   levelRaw <- B.readFile levelPath
   scenState <- testParser levelRaw
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
   -- link text buffer to ScenarioController
   let lst = createTextViewLink textBuffer :: UpdateListener MatrixScenario
   runStateT (handleController lst) (initControllerState scenState)
   return ()
   -- finalize window
   set window [ containerChild := textArea]
   _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
   widgetShowAll window
   mainGUI

handleController :: Scenario sc => UpdateListener sc -> StateT (ControllerState sc) IO ()
handleController lst = do addListener lst
                          tryMove <- runPlayerMove MRight
                          when (isJust tryMove) $
                               (liftIO . putStrLn . show . fromJust) tryMove
                          return ()

