{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module UnbalancedMap (UnbalancedMap(..)) where

import Data.Maybe
import FiniteMap
import Prelude hiding (lookup)

data UnbalancedMap k a = E
                       | T (UnbalancedMap k a) (k,a) (UnbalancedMap k a)
                         deriving (Show)

-- Exercise 2.6

{-
module FiniteMap (FiniteMap(..)) where
    class FiniteMap m k where
        empty  :: m k a
        bind   :: k -> a -> m k a -> m k a
        lookup :: k -> m k a -> Maybe a
-}

instance (Ord k) => FiniteMap UnbalancedMap k where
  empty = E

  bind k v E = T E (k,v) E
  bind xk xv m@(T a y@(yk,yv) b) =
    if xk < yk then
      T (bind xk xv a) y b
    else if xk > yk then
      T a y (bind xk xv b)
    else m

  {-
  -- version 1lookup xk E = Nothing
  lookup xk m@(T a y@(yk,yv) b) =
    if xk < yk then
      lookup xk a
    else if xk > yk then
      lookup xk b
    else
      Just yv
  -}

  -- version 2
  lookup xk m = lookup' Nothing xk m
      where lookup' Nothing        xk E = Nothing
            lookup' (Just (ck,cv)) xk E = if ck == xk then Just cv else Nothing
            lookup' c              xk (T a y@(yk,yv) b) =
                if xk <= yk then
                    lookup' (Just y) xk a
                else
                    lookup' c xk b
