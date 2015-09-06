{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, TemplateHaskell, RecordWildCards, MultiParamTypeClasses, FlexibleInstances #-}
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

import           Control.Lens
import           Control.Monad
import           Data.Array as A
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B hiding(ByteString)
import           Data.Maybe
import           Data.Typeable

import LensNaming

--import ShiftGame.Helpers

-- | @Features@ are the doodads that can be placed in a scenario.
data Feature = Wall    -- ^ static wall
             | Floor   -- ^ empty floor
             | Object  -- ^ object that can be moved around
             | Target  -- ^ target where objects should be shifted on
             | TargetX -- ^ target occupied by an object
             deriving (Eq, Enum, Bounded, Ord, Show, Read)

-- | Valid player move directions.
data PlayerMovement = MLeft | MRight | MUp | MDown deriving (Eq, Enum, Bounded, Show, Read)

-- | Possible movement reactions of the player character.
data CharacterReaction = RMove PlayerMovement | RShift PlayerMovement deriving (Eq, Show, Read)

-- | Reasons why a requested player move can be invalid
data DenyReason = PathBlocked  -- ^ The target @Feature@ can neither be walked on nor shifted
                | ShiftBlocked -- ^ The target @Feature@ can be shifted but the after next coordinate is blocked
                | OutsideWorld -- ^ The target coordinate resides outside of the scenario
                | NoAction     -- ^ No available action for undo or redo
                | ActionUnsupported -- ^ The desired feature is not supported
                | InvalidMove  -- ^ Generic failure that cannot be represented by the above
                deriving (Eq, Enum, Bounded, Show, Read)

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
--   
--   'Floor' as second argument is used for "erasing", e. g. when 'undo'ing a shift operation
--   (@Floor@ cannot be shifted, normally).
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

-- | Scenario coordinates /(x, y)/.
type Coord = (Int, Int)

-- | Extract direction from a move or shift.
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
class Scenario sc where
  createEmptyScenario :: sc
  -- | Returns the bounds of a @Scenario@.
  --   The returned values are the lowest /(x, y)/ and the highest /(x, y)/ indices.
  --   There is no guarantee that all coordinates between the bounds are valid.
  --   
  --   The default implementation searches through all values of 'listCoordinates'
  --   and should be defined by instances.
  getScenarioBounds :: sc -> (Coord, Coord)
  getScenarioBounds s = let (allX, allY) = (unzip . listCoordinates) s
    in ((minimum allX, minimum allY), (maximum allX, maximum allY))
  -- | Test if a coordinate is within the world.
  --
  --   Default implementation: Tests if 'getFeature' returns a value.
  isInside :: sc -> Coord -> Bool
  isInside sc = not . isNothing . getFeature sc
  -- | Lists all coordinates that is part of the scenario. Lists no coordinate twice.
  listCoordinates :: sc -> [Coord]
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

-- | Generic wrapper for any 'Scenario'.
-- 
-- ==== See also
-- @'convertScenario'@
data GenericScenario = forall s. (Typeable s, Scenario s) => GenericScenario { boxedScenario :: s } deriving (Typeable)
$(makeLensPrefixLenses ''GenericScenario)

instance Scenario (GenericScenario) where
  createEmptyScenario = GenericScenario (createEmptyScenario :: MatrixScenario)
  isInside (GenericScenario s) = isInside s
  listCoordinates (GenericScenario s) = listCoordinates s
  getFeature (GenericScenario s) = getFeature s
  setFeature (GenericScenario s) c ft = maybe Nothing (Just . GenericScenario) (setFeature s c ft)
  modifyFeature (GenericScenario s) c f = maybe Nothing (Just . GenericScenario) (modifyFeature s c f)

-- | 'Scenario' instance with an underlying dense matrix.
newtype MatrixScenario = MatrixScenario { scenarioMatrix :: Array Coord Feature } deriving(Eq, Show, Typeable)
$(makeLensPrefixLenses ''GenericScenario)

instance Scenario MatrixScenario where
  createEmptyScenario = MatrixScenario (A.listArray ((0,0), (0,0)) [Floor])
  isInside (MatrixScenario mat) c = inRange (bounds mat) c
  getScenarioBounds = bounds . scenarioMatrix
  listCoordinates (MatrixScenario mat) = A.indices mat
  getFeature sc@(MatrixScenario mat) c = if isInside sc c
                                           then return $ mat!c
                                           else Nothing
  setFeature sc@(MatrixScenario mat) c ft = if isInside sc c
                                           then return $ MatrixScenario $ mat//[(c, ft)]
                                           else Nothing

-- | Lists all valid coordinates of a @Scenario@ and the corresponding @Feature@s
getScenarioFeatures :: Scenario sc => sc -> [(Coord, Feature)]
getScenarioFeatures s = [(c, maybe Wall id (getFeature s c)) | c <- listCoordinates s]

-- | Single character representation of a @Feature@.
showFeature :: Feature -> Char
showFeature Wall    = '#'
showFeature Floor   = ' '
showFeature Object  = '$'
showFeature Target  = '.'
showFeature TargetX = '*'

-- | Character representation of the player on a certain feature.
combinePlayerAndFeature :: Feature -> Char
combinePlayerAndFeature Target =  '+'    -- Target
combinePlayerAndFeature Wall   =  '@'    -- Floor
combinePlayerAndFeature _      =  '@'    -- others are invalid, fall back

-- | Converts a @MatrtixScenario@ into an easily readable string.
showScenario :: MatrixScenario -> ByteString
showScenario (MatrixScenario mat) = fst $ B.unfoldrN ((lineLength) * (yh-yl+2)) seedFunc (xl, yl)
  --                                                                 #rows + 1 for line breaks
  where ((xl, yl), (xh, yh)) = bounds mat
        lineLength = xh - xl + 1
        seedFunc :: (Int, Int) -> Maybe (Char, (Int, Int))
        seedFunc c@(x, y)
          | y > yh      = Nothing                    -- finished last row
          | x == xh + 1 = Just ('\n', (xl, y+1))     -- end of row: linebreak and continue at next row
          | otherwise   = Just (showFeature (mat!c), (x+1, y))    -- next character in row

-- | Converts a @MatrtixScenario@ into an easily readable string and displays the player on the given coordinate.
showScenarioWithPlayer :: MatrixScenario -> Coord -> ByteString
showScenarioWithPlayer (MatrixScenario mat) pC = fst $ B.unfoldrN ((lineLength) * (yh-yl+2)) seedFunc (xl, yl)
  --                                                                 #rows + 1 for line breaks
  where ((xl, yl), (xh, yh)) = bounds mat
        lineLength = xh - xl + 1
        seedFunc :: Coord -> Maybe (Char, Coord)
        seedFunc c@(x, y)
          | y > yh      = Nothing                    -- finished last row
          | x == xh + 1 = Just ('\n', (xl, y+1))     -- end of row: linebreak and continue at next row
          | c == pC     = Just (combinePlayerAndFeature (mat!c), (x+1, y))
          | otherwise   = Just (showFeature (mat!c), (x+1, y))    -- next character in row

-- | A @ScenarioState@ stores the current state of a game.
data ScenarioState sc = ScenarioState
                        { playerCoord  :: Coord -- ^ current player coordinates
                        , scenario     :: sc    -- ^ current 'Scenario'
                        , emptyTargets :: Int   -- ^ the amount of unoccupied targets within the scenario
                        , spentSteps   :: (Int, Int)             -- ^ step counter excluding and including undos/redos
                        , pastMoveStack   :: [CharacterReaction] -- ^ player movements that led to the current state,
                                                                 --   first element is most recent action
                        , futureMoveQueue :: [CharacterReaction] -- ^ discarded movements for undone actions,
                                                                 --   first entry is the follow-up action
                        } deriving (Eq, Show, Read)
$(makeLensPrefixLenses ''ScenarioState)

-- | Helper class to change the type of a 'Scenario' within a 'ScenarioState'.
--   
--   Especially useful in conjunction with @ScenarioState 'GenericScenario'@.
class ConvertableScenarioState sc sc' where
  -- | Change the 'Scenario' type within the @ScenarioState@.
  convertScenarioState :: ScenarioState sc -> Maybe (ScenarioState sc')
  convertScenarioState (ScenarioState{..}) = case convertScenario scenario of 
      Just s -> Just $ (ScenarioState playerCoord s emptyTargets spentSteps pastMoveStack futureMoveQueue)
      Nothing -> Nothing
  -- | Converts the 'Scenario' type.
  convertScenario :: sc -> Maybe sc'

-- nothing to wrap if both types are equal
instance ConvertableScenarioState sc sc where
  convertScenarioState s = Just s
  convertScenario sc = Just sc

instance (Typeable sc, Scenario sc) => ConvertableScenarioState GenericScenario sc where
  convertScenario (GenericScenario sc) = cast sc

instance (Typeable sc, Scenario sc) => ConvertableScenarioState sc GenericScenario where
  convertScenario = Just . GenericScenario

-- make sure that most specialized type does not produce overhead
instance ConvertableScenarioState GenericScenario GenericScenario where
  convertScenarioState s = Just s
  convertScenario sc = Just sc

-- | Creates an empty @ScenarioState@.
emptyScenarioState :: Scenario sc => ScenarioState sc
emptyScenarioState = ScenarioState (0, 0) (createEmptyScenario) 0 (0, 0) [] []

-- | Tests if the @Scenario@ of a @ScenarioState@ is finished.
isWinningState :: ScenarioState sc -> Bool
isWinningState scs = emptyTargets scs == 0


-- | A storage for everything that changed within a 'ScenarioState'.
--   This includes the previous and current player position and their underlying 'Feature's.
--
-- ==== See also
-- @'askPlayerMove', 'updateScenario'@
data ScenarioUpdate = ScenarioUpdate
                      { changedFeatures :: [(Coord, Feature)] -- ^ a list of all changed @Features@, each coordinate is present only once
                                                              --   and the corresponding @Feature@ is the @Feature@ after the update
                      , newPlayerCoord  :: Coord              -- ^ the player coordinates after the update
                      , newEmptyTargets :: Int                -- ^ the total amount of unoccupied targets after the update
                      , updatedSteps    :: (Int, Int)         -- ^ effective and total number of steps (total number counts undos and redos)
                      , performedPlayerAction    :: CharacterReaction -- ^ the last performed action
                      } deriving (Eq, Show, Read)
$(makeLensPrefixLenses ''ScenarioUpdate)

-- | Tests whether a player move can be performed and computes the result.
--   Returns a @Left 'DenyReason'@ if the move is not possible and a
--   @Right 'ScenarioUpdate'@ with the resulting changes otherwise.
--
-- ==== See also
-- @'updateScenario'@
askPlayerMove :: Scenario sc => ScenarioState sc -> PlayerMovement -> Either DenyReason ScenarioUpdate
askPlayerMove scs dir =
    do let sc = scenario scs
           p = playerCoord scs                              -- player coord
           tp = moveCoordinate dir p                        -- move target coord
       if isInside sc tp
         then do let ft = fromJust $ getFeature sc tp       -- move target feature
                     cs = moveCoordinate dir tp             -- shift target coord
                     fs :: Maybe Feature
                     fs = getFeature sc cs                  -- shift target feature
                     (steps, steps') = spentSteps scs       -- effective and total steps so far
                 if walkable ft
                   then -- Move the player onto the target Feature
                        Right ScenarioUpdate { changedFeatures = [(p, fromMaybe Floor (getFeature sc p)), (tp, ft)]
                                             , newPlayerCoord = tp
                                             , newEmptyTargets = emptyTargets scs
                                             , updatedSteps = (steps + 1, steps' + 1)
                                             , performedPlayerAction = RMove dir }
                   else -- The target Feature cannot be walked on, but it may be shifted away
                        case (shiftable ft, (not . isNothing) fs && targetable (fromJust fs)) of
                             (True, True)  ->        -- perform a shift and move the player
                                  let (ft1, targetChange1) = combineFeatures ft            Floor  -- remove feature from current position
                                      (ft2, targetChange2) = combineFeatures (fromJust fs) ft     -- add feature to target position
                                  in Right  $ ScenarioUpdate { changedFeatures = [(p, fromMaybe Floor (getFeature sc p)), (tp, ft1), (cs, ft2)]
                                                             , newPlayerCoord = tp
                                                             , newEmptyTargets = emptyTargets scs + targetChange1 + targetChange2
                                                             , updatedSteps = (steps + 1, steps' + 1)
                                                             , performedPlayerAction = RShift dir}
                             (True, False) -> Left ShiftBlocked     -- Shift target space is blocked
                             _ -> Left PathBlocked                  -- Feature cannot be shifted
         else Left OutsideWorld


-- | Undo the last movement, returns the changed state or @Left NoAction@ if no previous action is recorded.
--
--   If the @DenyReason@ is not 'NoAction' it is an indicator for an inconsistent game state.
--
-- ==== See also
-- @'redo'@
undo :: Scenario sc => ScenarioState sc    -- ^ current scenario state
                    -> Either DenyReason (ScenarioUpdate, ScenarioState sc)    -- ^ on success: @ScenarioUpdate@ that leads to updated @ScenarioState@
undo scs = do
    (a, as) <- case pastMoveStack scs of                 -- action to undo
                    [] -> Left NoAction
                    a:as -> return (a, as)
    let dir = direction a                                -- last performed move direction
        backstep = revertMovement dir                    -- direction to move to previous direction
        oldpCoord = playerCoord scs                      -- current (=old) player coordinates
        newpCoord = moveCoordinate backstep oldpCoord    -- previous (=new) player position
        sc = scenario scs
        (steps, steps') = spentSteps scs                 -- current (effective steps, total steps)
    oldpFeature <- case getFeature sc oldpCoord of         -- feature at old player position
                      Just ft -> return ft
                      Nothing -> Left OutsideWorld
    newpFeature <- case getFeature sc newpCoord of        -- feature at new player position
                       Just ft -> return ft
                       Nothing -> Left OutsideWorld
    update <- case a of
                   RMove _ -> return $ ScenarioUpdate [(newpCoord, newpFeature), (oldpCoord, oldpFeature)]
                                                      newpCoord (emptyTargets scs) (steps-1, steps'+1) (a)
                   RShift _ -> do
                     let shiftfromCoord = moveCoordinate dir oldpCoord    -- coordinate where the shifted object came from
                     shiftfromFeature <- case getFeature sc shiftfromCoord of
                                              Just ft -> return ft
                                              Nothing -> Left InvalidMove
                     let (oldpFeature', chg) = combineFeatures oldpFeature shiftfromFeature      -- new feature at old player position
                         (shiftfromFeature', chg') = combineFeatures shiftfromFeature Floor      -- new feature where object has been shifte to
                     return $ ScenarioUpdate [(newpCoord, newpFeature), (oldpCoord, oldpFeature'), (shiftfromCoord, shiftfromFeature')]
                                             newpCoord (view lensEmptyTargets scs + chg + chg') (steps-1, steps'+1) (a)
    scs' <- fmap ((lensPastMoveStack .~ as) . (lensFutureMoveQueue .~ a : view lensFutureMoveQueue scs)) (updateScenario scs update)
    Right (update, scs')


-- | Redo the last undone movement, returns the changed state or @Left NoAction@ if no undone action is recorded.
--
--   If the @DenyReason@ is not 'NoAction' it is an indicator for an inconsistent game state.
--
-- ==== See also
-- @'undo'@
redo  :: Scenario sc => ScenarioState sc    -- ^ current scenario state
                     -> Either DenyReason (ScenarioUpdate, ScenarioState sc)    -- ^ on success: @ScenarioUpdate@ that leads to updated @ScenarioState@
redo scs = case futureMoveQueue scs of
                [] -> Left NoAction
                a:as -> let dir = direction a
                        in case askPlayerMove scs dir of
                                Left r -> Left r
                                Right update -> do
                                    scs' <- fmap (lensFutureMoveQueue .~ as) (updateScenario scs update)
                                    Right (update, scs')



-- | Performs a @ScenarioUpdate@ on the given @ScenarioState@.
--   The update is always possible if the @ScenarioUpdate@ has been computed via
--   'askPlayerMove' on the same @ScenarioState@.
--
-- ==== See also
-- @'askPlayerMove'@
updateScenario :: Scenario sc => ScenarioState sc -> ScenarioUpdate -> Either DenyReason (ScenarioState sc)
updateScenario scs u = do let sc = scenario scs
                              pcoord = newPlayerCoord u
                          nextPlayerCoord <- if isInside sc pcoord
                                               then Right pcoord
                                               else Left OutsideWorld
                          nextScenario <- case foldM (uncurry . setFeature) sc  (changedFeatures u) of
                                               Just sc' -> Right sc'
                                               Nothing  -> Left InvalidMove
                          return $ scs & lensPlayerCoord .~ nextPlayerCoord
                                      & lensScenario .~ nextScenario
                                      & lensEmptyTargets .~ newEmptyTargets u
                                      & lensSpentSteps .~ updatedSteps u
                                      & lensPastMoveStack %~ (performedPlayerAction u :)  -- append last action
                                      & lensFutureMoveQueue .~ []                         -- flush queue


