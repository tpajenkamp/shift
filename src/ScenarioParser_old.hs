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

{--
Parser overcomplicates things and is overkill since its special features are not needed
--}

module ScenarioParser{-- (
parseScenario, ParseState(..), initParseState
) --} where

import Prelude as P

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Lazy
import           Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import           Data.Maybe(fromMaybe, isNothing)
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
                  | InvalidCharacter Char Coord
                  | NoPlayerToken
                  | MultiplePlayerTokens
                  | LineEndedPrematurely Int
                  | LineTooLong Int
                  deriving (Eq, Show)

data ParseState = ParseState
                { linesReverse :: [V.Vector Feature]
                , linesCount   :: Int
                , userCoord    :: Maybe Coord
                , targetCount  :: Int
                , objectCount  :: Int
                , warnings     :: [ParseWarning]
                }

initParseState :: ParseState
initParseState = ParseState [] 0 Nothing  0 0 []

parseEntry :: Int -> StateT ParseState Parser Feature
parseEntry row = do ch <- lift anyChar
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
                         '*' -> do modify (addObject . addTarget)
                                   return TargetX
                         ' ' -> return Floor
                         _   -> do modify (\s -> s { warnings = InvalidCharacter ch (row, linesCount s) : warnings s })
                                   return Floor
  where setPlayer :: ParseState -> ParseState
        setPlayer s = if (isNothing . userCoord) s
                        then s { userCoord = Just (row, linesCount s) }
                        else s { warnings = MultiplePlayerTokens : warnings s }
        addTarget :: ParseState -> ParseState
        addTarget s = s { targetCount = 1 + targetCount s }
        addObject :: ParseState -> ParseState
        addObject s = s { objectCount = 1 + objectCount s }


parseLine :: StateT ParseState Parser ()
parseLine = do line <- lift takeLine
               let lineLength = B.length line
                   initLine = V.replicate lineLength Wall

                    --(lift (flip parse line)) :: StateT (-) Parser Result a

               newLine <- (lift (fromMaybe initLine . maybeResult . parse) . tokenParser initLine 0) line
               modify (\s -> s { linesReverse = newLine : linesReverse s
                               , linesCount = 1 + linesCount s })
 where takeLine :: Parser B.ByteString
       takeLine = A.takeWhile (\c -> c == '\n' || c == '\r')
       tokenParser :: V.Vector Feature -> Int -> StateT ParseState Parser (V.Vector Feature)
       tokenParser vec row = do ft <- parseEntry row
                                tokenParser (vec V.// [(row, ft)]) (row + 1) -- todo: does this end?


parseScenario :: StateT ParseState Parser (ScenarioState MatrixScenario)
parseScenario = do many $ parseLine <* lift endOfLine -- todo: inject line number
                   undefined
                   -- todo: check consistancy, create scenario

