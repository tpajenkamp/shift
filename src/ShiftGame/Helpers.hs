{-# LANGUAGE CPP #-}
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

module ShiftGame.Helpers (
    fromLeft, fromRight, mapTuple, mapTuple3
#if !MIN_VERSION_base(4,7,0)
    , isLeft, isRight
#endif
    ) where

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

#if !MIN_VERSION_base(4,7,0)
-- | Return `True` if the given value is a `Left`-value, `False` otherwise.
--
-- @since 4.7.0.0
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isLeft (Left "foo")
-- True
-- >>> isLeft (Right 3)
-- False
--
-- Assuming a 'Left' value signifies some sort of error, we can use
-- 'isLeft' to write a very simple error-reporting function that does
-- absolutely nothing in the case of success, and outputs \"ERROR\" if
-- any error occurred.
--
-- This example shows how 'isLeft' might be used to avoid pattern
-- matching when one does not care about the value contained in the
-- constructor:
--
-- >>> import Control.Monad ( when )
-- >>> let report e = when (isLeft e) $ putStrLn "ERROR"
-- >>> report (Right 1)
-- >>> report (Left "parse error")
-- ERROR
--
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

-- | Return `True` if the given value is a `Right`-value, `False` otherwise.
--
-- @since 4.7.0.0
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isRight (Left "foo")
-- False
-- >>> isRight (Right 3)
-- True
--
-- Assuming a 'Left' value signifies some sort of error, we can use
-- 'isRight' to write a very simple reporting function that only
-- outputs \"SUCCESS\" when a computation has succeeded.
--
-- This example shows how 'isRight' might be used to avoid pattern
-- matching when one does not care about the value contained in the
-- constructor:
--
-- >>> import Control.Monad ( when )
-- >>> let report e = when (isRight e) $ putStrLn "SUCCESS"
-- >>> report (Left "parse error")
-- >>> report (Right 1)
-- SUCCESS
--
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
#endif
