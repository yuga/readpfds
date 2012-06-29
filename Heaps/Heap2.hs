{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Heap2 (Heap2(..)) where

class (Ord a) => Heap2 h a where
  empty     :: h a
  isEmpty   :: h a -> Bool

  insert    :: a -> h a -> h a
  merge     :: h a -> h a -> h a

  findMin   :: h a -> a
  deleteMin :: h a -> h a
