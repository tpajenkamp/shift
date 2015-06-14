-----------------------------------------------------------------------------
--
-- Module      :  ShiftGame.Helpers
-- Copyright   :  (c) 2015, Thomas Pajenkamp
-- License     :  BSD3
--
-- Maintainer  :  tpajenka
-- Stability   :
-- Portability :
--
-- | Miscelleneous helper functions for basic types
--
-----------------------------------------------------------------------------

module ShiftGame.Helpers (fromLeft, fromRight, mapTuple, mapTuple3) where

-- Either functions
fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "Helpers.fromLeft: no Left"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Helpers.fromLeft: no Left"


-- Tuple functions
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (x, y, z) = (f x, f y, f z)