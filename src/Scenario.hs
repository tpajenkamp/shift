-----------------------------------------------------------------------------
--
-- Module      :  Scenario
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

module Scenario where

import           Control.Monad
import           Data.Array
import           Data.Foldable hiding (concat)
import           Data.Maybe

data Feature = Wall | Floor | Object | Target | TargetX deriving (Show, Eq, Enum)

data PlayerMovement = MLeft | MRight | MUp | MDown deriving (Eq, Show, Enum)

data DenyReason = PathBlocked | ShiftBlocked | OutsideWorld deriving (Show, Eq, Enum)

-- | Feature can be walked on?
walkable :: Feature -> Bool
walkable Floor  = True
walkable Target = True
walkable _      = False

-- | Feature can be shifted?
shiftable :: Feature -> Bool
shiftable Object  = True
shiftable TargetX = True
shiftable _       = False

-- | Features can be shifted onto this feature?
targetable :: Feature -> Bool
targetable Floor   = True
targetable Target  = True
targetable TargetX = False
targetable _       = False

combineFeatures :: Feature -> Feature -> (Feature, Int)
combineFeatures Target  Object  = (TargetX, -1)
combineFeatures Target  TargetX = (TargetX,  0)
combineFeatures Target  _       = (Target,   0)
combineFeatures TargetX Object  = (TargetX,  0)
combineFeatures TargetX TargetX = (TargetX,  0)
combineFeatures TargetX _       = (Target,   1)
combineFeatures _       new     = (new,      0)

type Coord = (Int, Int) -- ^ (x, y) coordinates

moveCoordinate :: PlayerMovement -> Coord -> Coord
moveCoordinate MLeft  (x, y) = (x-1, y)
moveCoordinate MRight (x, y) = (x+1, y)
moveCoordinate MUp    (x, y) = (x, y-1)
moveCoordinate MDown  (x, y) = (x, y+1)

class Scenario s where
  isInside :: Coord -> s -> Bool
  getFeature :: Coord -> s -> Maybe Feature
  setFeature :: Coord -> Feature -> s -> Maybe s
  modifyFeature :: Coord -> (Feature -> Feature) -> s -> Maybe s
  modifyFeature c f sc = do ft <- getFeature c sc
                            setFeature c ft sc

newtype MatrixScenario = MatrixScenario { matrix :: Array Coord Feature } deriving(Eq, Show)

showFeature :: Feature -> Char
showFeature Wall    = '#'
showFeature Floor   = ' '
showFeature Object  = '$'
showFeature Target  = '.'
showFeature TargetX = '+'

showScenario :: MatrixScenario -> String
showScenario (MatrixScenario mat) = let ((xl, yl), (xh, yh)) = bounds mat
                                    in do r <- [yl..yh]
                                          [ (showFeature . (!) mat) (c, r) | c <- [xl..xh] ] ++ "\n"


instance Scenario MatrixScenario where
  isInside c (MatrixScenario mat) = inRange (bounds mat) c
  getFeature c sc@(MatrixScenario mat) = if isInside c sc
                                           then return $ mat!c
                                           else Nothing
  setFeature c ft sc@(MatrixScenario mat) = if isInside c sc
                                           then return $ MatrixScenario $ mat//[(c, ft)]
                                           else Nothing


data ScenarioState sc = ScenarioState
                        { playerCoord  :: Coord
                        , scenario     :: sc
                        , emptyTargets :: Int
                        } deriving (Eq, Show)

isWinningState :: ScenarioState sc -> Bool
isWinningState st = emptyTargets st == 0


data ScenarioUpdate = ScenarioUpdate
                      { changedFeatures :: [(Coord, Feature)]
                      , newPlayerCoord  :: Coord
                      , newEmptyTargets :: Int
                      } deriving (Eq, Show)

askPlayerMove :: Scenario sc => PlayerMovement -> ScenarioState sc -> Either DenyReason ScenarioUpdate
askPlayerMove dir scs =
    do let sc = scenario scs
           p = playerCoord scs         -- player coord
           tp = moveCoordinate dir p   -- move target coord
       if isInside tp sc
         then do let ft = fromJust $ getFeature tp sc -- move target feature
                     cs = moveCoordinate dir tp       -- shift target coord
                     fs :: Maybe Feature
                     fs = getFeature cs sc            -- shift target feature
                 if walkable ft
                   then Right $ ScenarioUpdate { changedFeatures = []
                                               , newPlayerCoord = tp
                                               , newEmptyTargets = emptyTargets scs }
                   else case (shiftable ft, (not . isNothing) fs && targetable (fromJust fs)) of
                             (True, True)  -> Right $
                                  let (ft1, targetChange1) = combineFeatures ft            Floor
                                      (ft2, targetChange2) = combineFeatures (fromJust fs) ft
                                  in ScenarioUpdate { changedFeatures = [(tp, ft1), (cs, ft2)]
                                                    , newPlayerCoord = tp
                                                    , newEmptyTargets = emptyTargets scs + targetChange1 + targetChange2 }
                             (True, False) -> Left ShiftBlocked
                             _ -> Left PathBlocked
         else Left OutsideWorld


updateScenario :: Scenario sc => ScenarioState sc -> ScenarioUpdate -> ScenarioState sc
updateScenario sc u = sc
            { playerCoord = newPlayerCoord u
            , scenario = case foldM ((flip . uncurry) setFeature) (scenario sc) (changedFeatures u) of
                              Just sc' -> sc'
                              Nothing  -> error "invalid update step" -- Nothing -> sc
            , emptyTargets = newEmptyTargets u
            }


