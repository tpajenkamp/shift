module Helpers (fromLeft, fromRight) where

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "Helpers.fromLeft: no Left"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Helpers.fromLeft: no Left"
