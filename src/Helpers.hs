module Helpers (fromLeft, fromRight, mapTuple, mapTuple3) where

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