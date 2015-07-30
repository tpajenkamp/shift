module ShiftGame.ShiftIO where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B


import ShiftGame.Helpers
import ShiftGame.Scenario
import ShiftGame.ScenarioParser


displayScenarioData :: ScenarioState MatrixScenario -> IO ()
displayScenarioData sc = do
   putStrLn $ "player: " ++ (show . playerCoord) sc ++ " empty targets: " ++ (show . emptyTargets) sc
   (B.putStrLn . flip showScenarioWithPlayer (playerCoord sc) . scenario) sc

runParser :: ByteString -> IO [ScenarioState MatrixScenario]
runParser levelRaw = do let possiblyParsed = parseOnly (runStateT (parseScenarioCollection) initParseState) levelRaw
                        unless (isRight possiblyParsed) $
                            do guard False
                               (error . fromLeft) possiblyParsed
                        let (myScenarioStates, myParseState) = fromRight possiblyParsed
                        _ <- mapM evaluate myScenarioStates
                        putStrLn "warnings:"
                        putStrLn $ (unlines . map show . reverse . warnings) myParseState
                        mapM displayScenarioData myScenarioStates
                        return myScenarioStates -- todo: parse error


readScenario :: FilePath -> IO [ScenarioState MatrixScenario]
readScenario levelPath = do
   levelRaw <- catch (B.readFile levelPath) ((\e -> putStrLn ("failed to read level file " ++ levelPath) >> return B.empty)::IOError -> IO ByteString)
   runParser levelRaw

