{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module UnbalancedSet (UnbalancedSet(..)) where

import Control.Applicative
import Control.Monad.Error
import Data.Maybe
import FiniteMap
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

  {-
  -- Exercise 2.2 (version 1)
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
  -}

  {-
  -- Exercise 2.2 (version 2)
  member x tree = member' Nothing x tree
    where member' Nothing  x E = False
          member' (Just y) x E = x == y
          member' cand     x (T a y b)
            | x <= y    = member' (Just y) x a
            | otherwise = member' cand     x b
  -}

  -- Exercise 2.2 (version 2.1)
  member x tree = member' Nothing x tree
    where member' cand  x E                     = maybe False (==x) cand
          member' cand  x (T a y b) | x <= y    = member' (Just y) x a
                                    | otherwise = member' cand     x b

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
  -- Exercise 2.3 version 1 (Monad Error)
  insert x E = T E x E
  insert x s = case insertEx x s of
                 Right t             -> t
                 Left  AlreadyExists -> s
    where insertEx x E           = return $ T E x E
          insertEx x (T a y b)
            | x < y     = do t <- insertEx x a
                             return (T t y b)
            | x > y     = do t <- insertEx x b
                             return (T a y t)
            | otherwise = throwError AlreadyExists
  -}

  {-
  -- Exercise 2.3 version 2.1 (Maybe)
  insert x s = fromMaybe s (insertEx x s)
    where insertEx x E                     = Just $ T E x E
          insertEx x (T a y b) | x < y     = do t <- insertEx x a
                                                return $ T t y b
                               | x > y     = do t <- insertEx x b
                                                return $ T a y t
                               | otherwise = Nothing
  -}

  {-
  -- Exercise 2.3 version 2.2 (Maybe)
  insert x s = fromMaybe s (insertEx x s)
    where insertEx x E                     = Just $ T E x E
        --insertEx x (T a y b) | x < y     = ((flip.).flip) T y b <$> insertEx x a
          insertEx x (T a y b) | x < y     = (flip.(flip T)) y b <$> insertEx x a
                               | x > y     = T a y <$> insertEx x b
                               | otherwise = Nothing
  -}

  {-
  -- Exercise 2.4
  insert x E = T E x E
  insert x s = case insertEx1 x s of
                 Right t             -> t
                 Left  AlreadyExists -> s
    where insertEx1 x E  = return $ T E x E
          insertEx1 x (T a y b)
            | x <= y     = do t <- insertEx2 y x a
                              return (T t y b)
            | otherwise  = do t <- insertEx1 x b
                              return (T a y t)
          insertEx2 cand x E
            | cand == x  = throwError AlreadyExists
            | otherwise  = return (T E x E)
          insertEx2 cand x (T a y b)
            | x <= y     = do t <- insertEx2 y x a
                              return (T t y b)
            | otherwise  = do t <- insertEx2 cand x b
                              return (T a y t)
  -}

  -- Exercise 2.4 version 2.1 (Maybe)
  insert x s = fromMaybe s (insertEx Nothing x s)
    where insertEx (Just c) x E         | x == c    = Nothing
          insertEx _        x E                     = Just $ T E x E
          insertEx cand     x (T a y b) | x <= y    = do t <- insertEx (Just y) x a
                                                         return $ T t y b
                                        | otherwise = do t <- insertEx (Just y) x b
                                                         return $ T a y t

  {-
  -- Exercise 2.4 version 2.2 (Maybe and liftM)
  insert x s = fromMaybe s (insertEx Nothing x s)
    where insertEx (Just c) x E         | x == c    = Nothing
          insertEx _        x E                     = Just $ T E x E
          insertEx cand     x (T a y b) | x <= y    = (flip.(flip T)) y b <$> insertEx (Just y) x a
                                        | otherwise = T a y <$> insertEx cand x b
  -}

-- Exercise 2.5 (a)
complete x 0 = let s = E in T s x s
complete x d = let s = complete x (d-1) in T s x s

-- Exercise 2.5 (b)
complete1 x 0 = E
complete1 x 1 = T E x E
complete1 x n = let (d,r) = (n-1) `quotRem` 2
                in if r > 0 then
                     complete2 x d
                   else
                     let t = complete1 x d in T t x t
  where complete2 x 0 = T E x (complete1 x 1)
        complete2 x n = T (complete1 x n) x (complete1 x (n+1))
