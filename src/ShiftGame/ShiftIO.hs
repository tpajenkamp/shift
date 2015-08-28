{-# LANGUAGE ScopedTypeVariables #-}
module ShiftGame.ShiftIO where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.State.Lazy
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Either


import ShiftGame.Helpers
import ShiftGame.Scenario
import ShiftGame.ScenarioParser

-- | Shows the parsed scenario on stdout.
displayScenarioData :: ScenarioState MatrixScenario -> IO ()
displayScenarioData sc = do
   putStrLn $ "player: " ++ (show . playerCoord) sc ++ " empty targets: " ++ (show . emptyTargets) sc
   (B.putStrLn . flip showScenarioWithPlayer (playerCoord sc) . scenario) sc

-- | Parses levels from the given @ByteString@.
runParser :: ByteString -> IO [ScenarioState MatrixScenario]
runParser levelRaw = do let possiblyParsed = parseOnly (runStateT (parseScenarioCollection) initParseState) levelRaw
                        unless (isRight possiblyParsed) $
                            do guard False
                               (error . fromLeft) possiblyParsed
                        let (myScenarioStates, myParseState) = fromRight possiblyParsed
                        _ <- mapM evaluate myScenarioStates
                        putStrLn "warnings:"
                        putStrLn $ (unlines . map show . reverse . warnings) myParseState
                        _ <- mapM displayScenarioData myScenarioStates
                        return myScenarioStates -- todo: parse error


-- | Parses levels from the given file path. Returns @IO 'Nothing'@ on failure.
readScenario :: FilePath -> IO (Maybe [ScenarioState MatrixScenario])
readScenario levelPath = do
   mbFileData <- catch ((liftM Just) (B.readFile levelPath))
                       (\(_ :: IOException) -> putStrLn ("failed to read level file " ++ levelPath) >> return Nothing)
   maybe (return Nothing) (liftM Just . runParser) mbFileData

