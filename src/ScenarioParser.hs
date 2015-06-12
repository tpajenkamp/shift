-----------------------------------------------------------------------------
--
-- Module      :  ScenarioParser
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  tpajenka@foo
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module ScenarioParser{-- (
parseScenario, ParseState(..), initParseState
) --} where

--import           Prelude hiding ((//))

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Lazy
--import           Control.Monad.State.Lazy
import           Data.Array (Array, array)
import qualified Data.Array as A
import           Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.List (group, maximumBy, sort)
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Ord (comparing)
import           Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V

import Scenario



{--

Wall 	# 	0x23
Player 	@ 	0x40
Player on goal square 	+ 	0x2b
Box 	$ 	0x24
Box on goal square 	* 	0x2a
Goal square 	. 	0x2e
Floor 	(Space) 	0x20

--}


data ParseWarning = ObjectTargetMismatch Int Int
                  | InvalidCharacter Coord Char
                  | NoPlayerToken
                  | NoTarget
                  | MultiplePlayerTokens
                  | ScenarioEmpty
                  deriving (Eq, Show)

data ParseState = ParseState
                { linesReverse :: [Vector Feature]
                , linesCount   :: Int
                , userCoord    :: Maybe Coord
                , targetCount  :: Int
                , freeTargets  :: Int
                , objectCount  :: Int
                , warnings     :: [ParseWarning]
                } deriving (Eq, Show)

initParseState :: ParseState
initParseState = ParseState [] 0 Nothing  0 0 0 []

parseEntry :: Monad m => Int -> Char -> StateT ParseState m Feature
parseEntry row ch = case ch of
                         '#' -> return Wall
                         '@' -> do modify setPlayer
                                   return Floor
                         '+' -> do modify (setPlayer . addTarget)
                                   return Target
                         '.' -> do modify addTarget
                                   return Target
                         '$' -> do modify addObject
                                   return Object
                         '*' -> do modify (addOccupiedTarget)
                                   return TargetX
                         ' ' -> return Floor
                         _   -> do modify (\s -> s { warnings = InvalidCharacter (linesCount s, row) ch : warnings s })
                                   return Wall
  where setPlayer :: ParseState -> ParseState
        setPlayer s = if (isNothing . userCoord) s
                        then s { userCoord = Just (linesCount s, row) }
                        else s { warnings = MultiplePlayerTokens : warnings s }
        addTarget :: ParseState -> ParseState
        addTarget s = s { targetCount = 1 + targetCount s, freeTargets = 1 + freeTargets s }
        addOccupiedTarget :: ParseState -> ParseState
        addOccupiedTarget s = s { targetCount = 1 + targetCount s, objectCount = 1 + objectCount s }
        addObject :: ParseState -> ParseState
        addObject s = s { objectCount = 1 + objectCount s }


parseLine :: StateT ParseState Parser ()
parseLine = do line <- lift $ A.takeWhile (\c -> c /= '\n' && c /= '\r') -- break on line ends
               let lineLength = B.length line
               newLine <- V.generateM lineLength (tokenParser line)
               when ((not . V.null) newLine) $
                   modify (\s -> s { linesReverse = newLine : linesReverse s
                                   , linesCount = 1 + linesCount s })
  where tokenParser :: Monad m => ByteString -> Int -> StateT ParseState m Feature
        tokenParser line row = parseEntry row (line `B.index` row)


--mostFrequent :: Ord a => [a] -> a
--mostFrequent = head . maximumBy (comparing length) . group . sort

createScenarioArrayList :: Int -> Int -> [Vector Feature] -> [(Coord, Feature)]
createScenarioArrayList maxCol row (line:rem) = let lineMax = V.length line - 1
                                                    maxIx = min lineMax maxCol
                 in createScenarioArrayList maxCol (row - 1) rem ++
                        map (\c -> ((c, row), line!c)) [0..maxIx] ++
                        map (\c -> ((c, row), Wall)) [maxIx + 1 .. maxCol]
createScenarioArrayList _ _ [] = []

parseData :: StateT ParseState Parser ()
parseData = do parseLine
               nextChar <- lift peekChar
               case nextChar of
                    Nothing -> return ()
                    _ -> lift endOfLine <* parseData

parseScenario :: StateT ParseState Parser (ScenarioState MatrixScenario)
parseScenario = do parseData -- many $ parseLine <* lift endOfLine -- does not handle endOfInput
                   -- sanity tests
                   s <- get
                   when (targetCount s /= objectCount s) $
                       modify (\s -> s { warnings = ObjectTargetMismatch (objectCount s) (targetCount s) : warnings s })
                   s <- get
                   when (targetCount s == 0) $
                       modify (\s -> s { warnings = NoTarget : warnings s })
                   s <- get
                   when ((isNothing . userCoord) s) $
                       modify (\s -> s { warnings = NoPlayerToken : warnings s })
                   s <- get
                   let rowCounts = map V.length (linesReverse s)
                       rowLength = if (not . null) rowCounts then maximum rowCounts else 0
                   when ((null . linesReverse) s) $
                       modify (\s -> s { warnings = ScenarioEmpty : warnings s })
                   -- create scenario
                   s <- get
                   let rowMax = linesCount s - 1
                       colMax = rowLength - 1
                       arrayList = createScenarioArrayList colMax rowMax (linesReverse s)
                       scArray = array ((0,0), (colMax,rowMax)) arrayList
                   return $ ScenarioState (fromMaybe (0, 0) (userCoord s)) (MatrixScenario scArray) (freeTargets s)
