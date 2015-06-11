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


data Feature = Wall | Floor | Object | Target | TargetX deriving (Show, Eq, Enum)

data DenyReason = PathBlocked | ShiftBlocked deriving (Show, Eq, Enum)

walkable :: Feature -> Bool
walkable Floor  = True
walkable Target = True
walkable _      = False

shiftable :: Feature -> Bool
shiftable Floor   = True
shiftable Target  = True
shiftable TargetX = True
shiftable _       = False

combineFeatures :: Feature -> Feature -> (Feature, Int)
combineFeatures Target  Object  = (TargetX, -1)
combineFeatures Target  Target  = (Target,   0)
combineFeatures Target  new     = (new,      1) -- target destroyed, still counts as target to "fill"
combineFeatures TargetX Object  = (TargetX,  0)
combineFeatures TargetX Target  = (Target,   1)
combineFeatures TargetX TargetX = (TargetX,  0)
combineFeatures TargetX new     = (new,      1) -- target destroyed, still counts as target to "fill"
combineFeatures _       new     = (new,      0)

type Coord = (Int, Int) -- ^ (x, y) coordinates

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

performPlayerMove :: Scenario sc => Coord -> ScenarioState sc -> Either DenyReason ScenarioUpdate
performPlayerMove c sc = undefined --todo


updateScenario :: Scenario sc => ScenarioState sc -> ScenarioUpdate -> ScenarioState sc
updateScenario sc u = sc
            { playerCoord = newPlayerCoord u
            , scenario = case foldM ((flip . uncurry) setFeature) (scenario sc) (changedFeatures u) of
                              Just sc' -> sc'
                              Nothing  -> error "invalid update step" -- Nothing -> sc
            , emptyTargets = newEmptyTargets u
            }


