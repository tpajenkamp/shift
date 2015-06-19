-----------------------------------------------------------------------------
--
-- Module      :  ShiftGame.Scenario
-- Copyright   :  (c) 2015, Thomas Pajenkamp
-- License     :  BSD3
--
-- Maintainer  :  tpajenka
-- Stability   :
-- Portability :
--
-- | Game data and model
--
-----------------------------------------------------------------------------

module ShiftGame.Scenario where

import           Control.Monad
import           Data.Array
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B hiding(ByteString)
import           Data.Maybe

--import ShiftGame.Helpers

-- | @Features@ are the doodads that can be placed in a scenario.
data Feature = Wall    -- ^ static wall
             | Floor   -- ^ empty floor
             | Object  -- ^ object that can be moved around
             | Target  -- ^ target where objects should be shifted on
             | TargetX -- ^ target occupied by an object
             deriving (Eq, Enum, Show, Read)

-- | Valid player move directions.
data PlayerMovement = MLeft | MRight | MUp | MDown deriving (Eq, Enum, Show, Read)

-- | Possible movement reactions of the player character.
data CharacterReaction = RMove PlayerMovement | RShift PlayerMovement deriving (Eq, Show, Read)

-- | Reasons why a requested player move can be invalid
data DenyReason = PathBlocked  -- ^ The target @Feature@ can neither be walked on nor shifted
                | ShiftBlocked -- ^ The target @Feature@ can be shifted but the after next coordinate is blocked
                | OutsideWorld -- ^ The target coordinate resides outside of the scenario
                | NoAction     -- ^ No available action for undo or redo
                | ActionUnsupported -- ^ The desired feature is not supported
                | InvalidMove  -- ^ Generic failure that cannot be represented by the above
                deriving (Eq, Enum, Show, Read)

-- | @Feature@ can be walked on?
walkable :: Feature -> Bool
walkable Floor  = True
walkable Target = True
walkable _      = False

-- | @Feature@ can be shifted onto other @Features@?
shiftable :: Feature -> Bool
shiftable Object  = True
shiftable TargetX = True
shiftable _       = False

-- | Other @Features@ can be shifted onto this @Feature@?
targetable :: Feature -> Bool
targetable Floor   = True
targetable Target  = True
targetable TargetX = False
targetable _       = False

-- | What you get if you mix two @Feature@s.
combineFeatures :: Feature        -- ^ previous @Feature@
                -> Feature        -- ^ new @Feature@
                -> (Feature, Int) -- ^ the combined @Feature@ and the change of unoccupied targets
combineFeatures Target  Object  = (TargetX, -1)
combineFeatures Target  TargetX = (TargetX, -1)
combineFeatures Target  _       = (Target,   0)
combineFeatures TargetX Object  = (TargetX,  0)
combineFeatures TargetX TargetX = (TargetX,  0)
combineFeatures TargetX _       = (Target,   1)
combineFeatures _       TargetX = (Object,   0)
combineFeatures _       new     = (new,      0)

-- | Scenario coordinates @(x, y)@.
type Coord = (Int, Int)

direction :: CharacterReaction -> PlayerMovement
direction (RMove dir) = dir
direction (RShift dir) = dir

-- | The opposite direction of a movement.
revertMovement :: PlayerMovement -> PlayerMovement
revertMovement MLeft   = MRight
revertMovement MRight = MLeft
revertMovement MUp    = MDown
revertMovement MDown  = MUp

-- | Coordinate movement.
moveCoordinate :: PlayerMovement -- ^ move direction
               -> Coord          -- ^ original coordinate
               -> Coord          -- ^ target coordinate
moveCoordinate MLeft  (x, y) = (x-1, y)
moveCoordinate MRight (x, y) = (x+1, y)
moveCoordinate MUp    (x, y) = (x, y-1)
moveCoordinate MDown  (x, y) = (x, y+1)

-- | A @Scenario@ is a possibly bounded world of 'Feature's.
--   Each feature coordinate may be changed.
--   
--  === Minimal complete definition
--  > getFeature', 'setFeature'
--  Definition of 'isInside' is recommended.
class Scenario sc where
  -- | Test if a coordinate is within the world.
  isInside :: sc -> Coord -> Bool
  isInside sc = not . isNothing . getFeature sc
  -- | Get the @Feature@ at the specified coordinates.
  --   Returns 'Nothing' if the coordinates are inccassible.
  getFeature :: sc -> Coord -> Maybe Feature
  -- | Set the @Feature@ at the specified coordinates.
  --   Returns 'Nothing' if the coordinates are inccassible, the updated scenario otherwise.
  setFeature :: sc -> Coord -> Feature -> Maybe sc
  -- | Changes the @Feature@ at the specified coordinates depending on the current @Feature@.
  --   Returns 'Nothing' if the coordinates are inccassible, the updated scenario otherwise.
  modifyFeature :: sc -> Coord -> (Feature -> Feature) -> Maybe sc
  modifyFeature sc c f = do ft <- getFeature sc c
                            setFeature sc c (f ft)

-- | 'Scenario' instance with an underlying dense matrix.
newtype MatrixScenario = MatrixScenario { matrix :: Array Coord Feature } deriving(Eq, Show)


instance Scenario MatrixScenario where
  isInside (MatrixScenario mat) c = inRange (bounds mat) c
  getFeature sc@(MatrixScenario mat) c = if isInside sc c
                                           then return $ mat!c
                                           else Nothing
  setFeature sc@(MatrixScenario mat) c ft = if isInside sc c
                                           then return $ MatrixScenario $ mat//[(c, ft)]
                                           else Nothing

-- | Returns the internal matrix of a @MatrixScenario@.
getMatrixScenarioArray :: MatrixScenario -> Array Coord Feature
getMatrixScenarioArray (MatrixScenario a) = a

-- | Returns the bounds of a @MatrixScenario@.
--   The returned values are the lower @(x, y)@ and the upper @(x, y)@ bounds.
--   All values between the bounds, including the bounds themselves, are valid coordinates for the scenario.
getMatrixScenarioBounds :: MatrixScenario -> (Coord, Coord)
getMatrixScenarioBounds = bounds . getMatrixScenarioArray

-- | Single character representation of a @Feature@.
showFeature :: Feature -> Char
showFeature Wall    = '#'
showFeature Floor   = ' '
showFeature Object  = '$'
showFeature Target  = '.'
showFeature TargetX = '*'

combinePlayerAndFeature :: Feature -> Char
combinePlayerAndFeature Target =  '+'    -- Target
combinePlayerAndFeature Wall   =  '@'    -- Floor
combinePlayerAndFeature _      =  '@'    -- others are invalid, fall back

-- | Converts a @MatrtixScenario@ into a easily readable string.
showScenario :: MatrixScenario -> ByteString
showScenario (MatrixScenario mat) = fst $ B.unfoldrN ((lineLength) * (yh-yl+2)) seedFunc (0, 0)
  --                                                                 #rows + 1 for line breaks
  where ((xl, yl), (xh, yh)) = bounds mat
        lineLength = xh - xl + 1
        seedFunc :: (Int, Int) -> Maybe (Char, (Int, Int))
        seedFunc c@(x, y)
          | y > yh      = Nothing                   -- finished last row
          | x == xh + 1 = Just ('\n', (xl, y+1))     -- end of row: linebreak and continue at next row
          | otherwise   = Just (showFeature (mat!c), (x+1, y))    -- next character in row

showScenarioWithPlayer :: MatrixScenario -> Coord -> ByteString
showScenarioWithPlayer (MatrixScenario mat) pC = fst $ B.unfoldrN ((lineLength) * (yh-yl+2)) seedFunc (0, 0)
  --                                                                 #rows + 1 for line breaks
  where ((xl, yl), (xh, yh)) = bounds mat
        lineLength = xh - xl + 1
        seedFunc :: Coord -> Maybe (Char, Coord)
        seedFunc c@(x, y)
          | y > yh      = Nothing                   -- finished last row
          | x == xh + 1 = Just ('\n', (xl, y+1))     -- end of row: linebreak and continue at next row
          | c == pC     = Just (combinePlayerAndFeature (mat!c), (x+1, y))
          | otherwise   = Just (showFeature (mat!c), (x+1, y))    -- next character in row

-- | A @ScenarioState@ stores the current state of a game.
data ScenarioState sc = ScenarioState
                        { playerCoord  :: Coord -- ^ current player coordinates
                        , scenario     :: sc    -- ^ current 'Scenario'
                        , emptyTargets :: Int   -- ^ the amount of unoccupied targets within the scenario
                        , pastMoveStack   :: [CharacterReaction] -- ^ player movements that led to the current state,
                                                                 --   first element is most recent action
                        , futureMoveQueue :: [CharacterReaction] -- ^ discarded movements for undone actions,
                                                                 --   first entry is the follow-up action
                        } deriving (Eq, Show, Read)

-- | Undo the last movement, returns the changed state or 'Nothing' if no previous action is recorded.
undo :: Scenario sc => ScenarioState sc -> Either DenyReason (ScenarioUpdate, ScenarioState sc)
undo scs = do 
    a <- case pastMoveStack scs of
              [] -> Left NoAction
              a:_ -> return a
    let dir = direction a
        backstep = revertMovement dir
        playercoord = playerCoord scs
        backcoord = moveCoordinate backstep playercoord
        sc = scenario scs
    plFeature <- case getFeature sc playercoord of
                      Just ft -> return ft
                      Nothing -> Left OutsideWorld
    plFeature' <- case getFeature sc backcoord of
                       Just ft -> return ft
                       Nothing -> Left OutsideWorld
    unless (walkable plFeature') $
        Left PathBlocked
    update <- case a of
                   (RMove _) -> return $ ScenarioUpdate [] backcoord (emptyTargets scs) Nothing
                   (RShift _) -> do
                       let backshiftFrom = moveCoordinate dir playercoord
                       backshiftFeature <- case getFeature sc backshiftFrom of
                                                Just ft -> return ft
                                                Nothing -> Left OutsideWorld
                       unless (targetable plFeature) $
                           Left ShiftBlocked
                       unless (shiftable backshiftFeature) $
                           Left InvalidMove
                       let (newFeatureP, chg)   = combineFeatures plFeature backshiftFeature
                           (newFeatureP', chg') = combineFeatures backshiftFeature Floor
                       return $ ScenarioUpdate [(playercoord, newFeatureP), (backshiftFrom, newFeatureP')]
                                               backcoord (emptyTargets scs + chg + chg') Nothing
    scs' <- updateScenario (scs { pastMoveStack = (tail . pastMoveStack) scs
                                , futureMoveQueue =  a : futureMoveQueue scs
                                }) update
    Right (update, scs')


redo  :: Scenario sc => ScenarioState sc -> Either DenyReason (ScenarioUpdate, ScenarioState sc)
redo scs = case futureMoveQueue scs of
                [] -> Left NoAction
                a:_ -> let dir = direction a
                       in case askPlayerMove scs dir of
                               Left r -> Left r
                               Right update -> do
                                   let update' = update { performedPlayerAction = Nothing }
                                   scs' <- updateScenario (scs { pastMoveStack = a : pastMoveStack scs
                                                               , futureMoveQueue = (tail . futureMoveQueue) scs
                                                               }) update'
                                   Right (update', scs')


-- | Tests if the @Scenario@ of a @ScenarioState@ is finished.
isWinningState :: ScenarioState sc -> Bool
isWinningState scs = emptyTargets scs == 0

-- | A storage for everything that changed within a 'ScenarioState'.
-- === See also
-- > 'askPlayerMove', 'updateScenario'
data ScenarioUpdate = ScenarioUpdate
                      { changedFeatures :: [(Coord, Feature)] -- ^ a list of all changed @Features@, each coordinate is present only once
                                                              --   and the corresponding @Feature@ is the @Feature@ after the update
                      , newPlayerCoord  :: Coord              -- ^ the player coordinates after the update
                      , newEmptyTargets :: Int                -- ^ the total amount of unoccupied targets after the update
                      , performedPlayerAction    :: Maybe CharacterReaction -- ^ the action type or @Nothing@ if it should not change the stored movement queue
                      } deriving (Eq, Show, Read)

-- | Tests whether a player move can be performed and computes the result.
--   Returns a @Left 'DenyReason'@ if the move is not possible and a
--   @Right 'ScenarioUpdate'@ with the resulting changes otherwise.
-- === See also
-- > 'updateScenario'
askPlayerMove :: Scenario sc => ScenarioState sc -> PlayerMovement -> Either DenyReason ScenarioUpdate
askPlayerMove scs dir =
    do let sc = scenario scs
           p = playerCoord scs                           -- player coord
           tp = moveCoordinate dir p                     -- move target coord
       if isInside sc tp
         then do let ft = fromJust $ getFeature sc tp    -- move target feature
                     cs = moveCoordinate dir tp          -- shift target coord
                     fs :: Maybe Feature
                     fs = getFeature sc cs               -- shift target feature
                 if walkable ft
                   then -- Move the player onto the target Feature
                        Right ScenarioUpdate { changedFeatures = []
                                             , newPlayerCoord = tp
                                             , newEmptyTargets = emptyTargets scs
                                             , performedPlayerAction = (Just . RMove) dir }
                   else -- The target Feature cannot be walked on, but it may be shifted away
                        case (shiftable ft, (not . isNothing) fs && targetable (fromJust fs)) of
                             (True, True)  ->        -- perform a shift and move the player
                                  let (ft1, targetChange1) = combineFeatures ft            Floor
                                      (ft2, targetChange2) = combineFeatures (fromJust fs) ft
                                  in Right  $ ScenarioUpdate { changedFeatures = [(tp, ft1), (cs, ft2)]
                                                             , newPlayerCoord = tp
                                                             , newEmptyTargets = emptyTargets scs + targetChange1 + targetChange2
                                                             , performedPlayerAction = (Just . RShift) dir}
                             (True, False) -> Left ShiftBlocked     -- Shift target space is blocked
                             _ -> Left PathBlocked                  -- Feature cannot be shifted
         else Left OutsideWorld


-- | Performs a @ScenarioUpdate@ on the given @ScenarioState@.
--   The update is always possible if the @ScenarioUpdate@ has been computed via
--   'askPlayerMove' on the same @ScenarioState@.
-- === See also
-- > 'askPlayerMove'
updateScenario :: Scenario sc => ScenarioState sc -> ScenarioUpdate -> Either DenyReason (ScenarioState sc)
updateScenario scs u = do let sc = scenario scs
                              pcoord = newPlayerCoord u
                          nextPlayerCoord <- if isInside sc pcoord
                                               then Right pcoord
                                               else Left OutsideWorld
                          nextScenario <- case foldM (uncurry . setFeature) (scenario scs)  (changedFeatures u) of
                                               Just sc' -> Right sc'
                                               Nothing  -> Left InvalidMove
                          return scs { playerCoord = nextPlayerCoord
                                     , scenario = nextScenario
                                     , emptyTargets = newEmptyTargets u
                                     , pastMoveStack = case performedPlayerAction u of
                                                            Nothing -> pastMoveStack scs
                                                            Just a  -> a : pastMoveStack scs
                                     , futureMoveQueue = case performedPlayerAction u of
                                                              Nothing -> futureMoveQueue scs
                                                              Just _  -> []
                                     }

