-----------------------------------------------------------------------------
--
-- Module      :  ShiftGame.ScenarioParser
-- Copyright   :  (c) 2015, Thomas Pajenkamp
-- License     :  BSD3
--
-- Maintainer  :  tpajenka
-- Stability   :
-- Portability :
--
-- | 'MatrixScenario' parser for single-level or batch level texts.
--
-----------------------------------------------------------------------------

module ShiftGame.ScenarioParser (
parseScenario, parseScenarioCollection, LevelId, ParseState(), warnings, levelCount, ParseWarning(..), initParseState
) where

--import           Prelude hiding ((//))

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Lazy
--import           Control.Monad.State.Lazy
import           Data.Array  as A (array)
import qualified Data.Attoparsec.ByteString as AP
import           Data.Attoparsec.ByteString.Char8 as APC
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (fromMaybe, isNothing, catMaybes)
import           Data.Vector as V (Vector, (!))
import qualified Data.Vector as V hiding (Vector, (!))

import ShiftGame.Scenario


{-
Wall    #     0x23
Player  @     0x40
PlayerX +     0x2b
Object  $     0x24
TargetX *     0x2a
Target  .     0x2e
Floor (Space) 0x20

-}

validChar :: Char -> Bool
validChar c = elem c "#@+.$* "

type LevelId = Int

-- | Warnings and errors for parsing a scenario file.
data ParseWarning = ObjectTargetMismatch LevelId Int Int   -- ^ number of objects that do not fit number of targets: level id, number of objects, number of targets
                  | InvalidCharacter LevelId Coord Char    -- ^ unknown symbol in level file: level id, symbol position, symbol
                  | NoPlayerToken LevelId                  -- ^ no player position specified in level: level id
                  | NoTarget LevelId                       -- ^ no target position specified in level: level id
                  | MultiplePlayerTokens LevelId           -- ^ multiple player positions specified in level: level id
                  | ScenarioEmpty LevelId                  -- ^ specified scenario is empty: level id
                  deriving (Eq, Show, Read)

-- | Internal state while parsing
data ParseState = ParseState
                { linesReverse :: [Vector Feature] -- ^ all level rows, rows in reverse order; row lengthses do not need to match
                , linesCount   :: Int              -- ^ current number of level rows, should be equal to @length linesReverse@
                , userCoord    :: Maybe Coord      -- ^ player coordinates (if specified)
                , targetCount  :: Int              -- ^ current number of target features ('Target' and 'TargetX')
                , freeTargets  :: Int              -- ^ current number of free target features ('Target' only)
                , objectCount  :: Int              -- ^ current number of shiftable objects ('Object' and 'TargetX')
                , warnings     :: [ParseWarning]   -- ^ occurred warnigns so far
                , levelCount   :: LevelId          -- ^ level counter, starting with 1
                } deriving (Eq, Show, Read)

-- | Initial empty 'ParseState'.
initParseState :: ParseState
initParseState = ParseState [] 0 Nothing  0 0 0 [] 1

-- | Reset level specific data such that a new scenario can be parsed, keeping @warnings@ and @currentLevel@.
resetParseState :: (Monad m) => StateT ParseState m ()
resetParseState = modify (\s -> s { linesReverse = []
                                  , linesCount = 0
                                  , userCoord = Nothing
                                  , targetCount = 0
                                  , freeTargets = 0
                                  , objectCount = 0})

-- | Parses a single 'Feature' and modifies the 'ParseState' accordingly.
parseEntry :: Monad m => Int   -- ^ current column
                      -> Char  -- ^ character to parse
                      -> StateT ParseState m Feature
parseEntry col ch = do
   s <- get
   let levelId = levelCount s
   case ch of
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
        _   -> do modify (\s -> s { warnings = InvalidCharacter levelId (col, linesCount s) ch : warnings s })
                  return Wall
  where -- Add player corrdinates to state.
        setPlayer :: ParseState -> ParseState
        setPlayer s = if (isNothing . userCoord) s
                        then s { userCoord = Just (col, linesCount s) }
                        else s { warnings = MultiplePlayerTokens (levelCount s) : warnings s }
        -- Increment target count within state.
        addTarget :: ParseState -> ParseState
        addTarget s = s { targetCount = 1 + targetCount s, freeTargets = 1 + freeTargets s }
        -- Increment target and object count within state.
        addOccupiedTarget :: ParseState -> ParseState
        addOccupiedTarget s = s { targetCount = 1 + targetCount s, objectCount = 1 + objectCount s }
        -- Increment object count within state.
        addObject :: ParseState -> ParseState
        addObject s = s { objectCount = 1 + objectCount s }


-- | Parses a single line of a level string.
--   Ignores empty lines.
parseLine :: StateT ParseState Parser Bool
parseLine = do line <- lift $ APC.takeWhile1 (\c -> c /= '\n' && c /= '\r') -- break on line ends
               let lineLength = B.length line
               newLine <- V.generateM lineLength (tokenParser line)   -- new row vector has fitting line length
               when ((not . V.null) newLine) $                        -- add row only if it is not empty
                   modify (\s -> s { linesReverse = newLine : linesReverse s
                                   , linesCount = 1 + linesCount s })
               lift $ endOfLine <|> endOfInput
               return True
  where tokenParser :: Monad m => ByteString -> Int -> StateT ParseState m Feature
        tokenParser line col = parseEntry col (line `B.index` col)


-- | Creates a dense matrix @[((c,r), value]@ that can be passed to the 'array' generator function.
--   If a row ends prematurely the missing values are filled with 'Wall'.
createScenarioArrayList :: Int                -- ^ number of columns in matrix, vector entries beyond are ignored
                        -> Int                -- ^ current row (index starts with @0@),provide last row of matrix first
                        -> [Vector Feature]   -- ^ rows of data in reverse order
                        -> [(Coord, Feature)]
createScenarioArrayList maxCol row (line:ls) = let lineMax = V.length line - 1
                                                   maxIx = min lineMax maxCol
                 in createScenarioArrayList maxCol (row - 1) ls ++
                        map (\c -> ((c, row), line!c)) [0..maxIx] ++
                        map (\c -> ((c, row), Wall)) [maxIx + 1 .. maxCol]
createScenarioArrayList _ _ [] = []

-- | Consumes a comment line. A comment line is a nonempty line that does not start with a valid level character.
parseTestCommentLine :: StateT ParseState Parser Bool
parseTestCommentLine = do
   _ <- lift $ satisfy (not . validChar)
   lift $ AP.skipWhile (not . isEndOfLine)
   lift $ endOfLine <|> endOfInput
   return False

parseTestEmptyLine :: StateT ParseState Parser Bool
parseTestEmptyLine = do
   lift $ endOfLine
   return False

-- | Parses string line by line and end at comment or end of input.
parseData :: StateT ParseState Parser ()
parseData = do continue <- (parseTestEmptyLine <|> parseTestCommentLine <|> parseLine)
               if continue
                 then parseData <|> lift (endOfInput >> return ())
                 else return ()

-- | Parses a level string.
--   Does not fail, the state's 'warnings' field may provide hints for the validity of the string.
--   If no player position could be found it is set to @(0, 0)@ and the 'NoPlayerToken' warning is added.
--   The first found player token (scanning rowwise) is selected.
parseScenario :: StateT ParseState Parser (Maybe (ScenarioState MatrixScenario))
parseScenario = do
   parseData
   s <- get
   let levelId = levelCount s
   -- skip empty scenarios
   if ((null . linesReverse) s)
      then return Nothing
      else do   -- Just scenario
          -- sanity tests
          when (targetCount s /= objectCount s) $
             modify (\s' -> s' { warnings = ObjectTargetMismatch levelId (objectCount s') (targetCount s') : warnings s' })
          s <- get
          when (targetCount s == 0) $
             modify (\s' -> s' { warnings = NoTarget levelId : warnings s' })
          s <- get
          when ((isNothing . userCoord) s) $
             modify (\s' -> s' { warnings = NoPlayerToken levelId : warnings s' })
          s <- get
          let rowCounts = map V.length (linesReverse s)
              rowLength = if (not . null) rowCounts then maximum rowCounts else 0
          when (rowLength == 0) $
             modify (\s' -> s' { warnings = ScenarioEmpty levelId : warnings s' })
          -- create scenario
          s <- get
          let rowMax = linesCount s - 1
              colMax = rowLength - 1
              arrayList = createScenarioArrayList colMax rowMax (linesReverse s)
              scArray = array ((0, 0), (colMax, rowMax)) arrayList
          modify (\s -> s { levelCount = levelCount s + 1 })
          resetParseState
          return $ Just $ ScenarioState (fromMaybe (0, 0) (userCoord s)) (MatrixScenario scArray) (freeTargets s) (0, 0) [] []

parseScenarioCollection :: StateT ParseState Parser [ScenarioState MatrixScenario]
parseScenarioCollection = do
   scens <- (liftA catMaybes) $ many (parseScenario)
   when (null scens) $
       modify (\s' -> s' { warnings = ScenarioEmpty 0 : warnings s' })
   return scens
