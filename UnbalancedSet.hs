{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module UnbalancedSet (UnbalancedSet(..)) where

import Control.Monad.Error
import Prelude hiding (lookup)
import Set


data SetError = AlreadyExists
                deriving (Eq, Show)

instance Error SetError

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)
                     deriving (Show)

instance (Ord a) => Set UnbalancedSet a where
  empty = E

  {-
  -- Original
  member x E = False
  member x (T a y b) =
    if x < y then
        member x a
    else if x > y then
        member x b
    else True
  -}

  -- Exercise 2.2
  member x E = False
  member x (T a y b) =
    if x <= y then
      meb y x a
    else
      member x b
    where meb cand x E = cand == x
          meb cand x (T a y b)
            | x <= y    = meb y x a
            | otherwise = meb cand x b

  {-
  -- Original
  insert x E = T E x E
  insert x s@(T a y b) =
    if x < y then
      T (insert x a) y b
    else if x > y then
      T a y (insert x b)
    else s
  -}

  {-
  -- Exercise 2.3
  insert x E = T E x E
  insert x s = case insertEx x s of
                 Right t             -> t
                 Left  AlreadyExists -> s
    where insertEx x E           = return $ T E x E
          insertEx x s@(T a y b)
            | x < y     = do t <- insertEx x a
                             return (T t y b)
            | x > y     = do t <- insertEx x b
                             return (T a y t)
            | otherwise = throwError AlreadyExists
  -}

  -- Exercise 2.4
  insert x E = T E x E
  insert x s = case insertEx1 x s of
                 Right t             -> t
                 Left  AlreadyExists -> s
    where insertEx1 x E  = return $ T E x E
          insertEx1 x s@(T a y b)
            | x <= y     = do t <- insertEx2 y x a
                              return (T t y b)
            | otherwise  = do t <- insertEx1 x b
                              return (T a y t)
          insertEx2 cand x E
            | cand == x  = throwError AlreadyExists
            | otherwise  = return (T E x E)
          insertEx2 cand x s@(T a y b)
            | x <= y     = do t <- insertEx2 y x a
                              return (T t y b)
            | otherwise  = do t <- insertEx2 cand x b
                              return (T a y t)

-- Exercise 2.5 (a)
complete x 0 = T E x E
complete x d = let s = complete x (d-1)
               in T s x s

-- Exercise 2.5 (b)
complete1 x 0 = E
complete1 x 1 = T E x E
complete1 x n = let (d,r) = (n-1) `quotRem` 2
                in if r > 0 then
                     complete2 x d
                   else
                     let t = complete1 x d
                     in T t x t
  where complete2 x 0 = T E x (complete1 x 1)
        complete2 x n = T (complete1 x n) x (complete1 x (n+1))
