module PairingHeap (PairingHeap) where

import Heap

data PairingHeap a = E
                   | T a [PairingHeap a]
                   deriving (Show)

data BinTree a = E'
               | T' (BinTree a) a (BinTree a)
               deriving (Show)

mergePairs :: (Ord a) => [PairingHeap a] -> PairingHeap a
mergePairs []  = E
mergePairs [h] = h
mergePairs (h1:h2:hs) =
    merge (merge h1 h2) $ mergePairs hs

toBinary :: PairingHeap a -> BinTree a
toBinary E = E'
toBinary (T x hs) = T' (tree hs) x E'
    where
      tree [] = E'
      tree ((T x hs1):hs2) = T' (tree hs1) x (tree hs2)

instance Heap PairingHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    insert x h = merge (T x []) h

    merge h E  = h
    merge E h = h
    merge h1@(T x hs1) h2@(T y hs2)
        | x <= y    = T x (h2:hs1)
        | otherwise = T y (h1:hs2)

    findMin E = error "empty heap"
    findMin (T x _) = x

    deleteMin E = error "empty heap"
    deleteMin (T x hs) = mergePairs hs

instance Heap BinTree where
    empty = E'

    isEmpty E' = True
    isEmpty _  = False

    insert x h = merge (T' E' x E') h

    merge h  E' = h
    merge E' h  = h
    merge (T' lh1 x _) (T' lh2 y _)
          | x <= y    = T' (T' lh2 y lh1) x E'
          | otherwise = T' (T' lh1 x lh2) y E'

    findMin E' = error "empty heap"
    findMin (T' _ x _) = x

    deleteMin E' = error "empty heap"
    deleteMin (T' h _ _) = mergePairs h
        where
          mergePairs :: (Ord a) => BinTree a -> BinTree a
          mergePairs E' = E'
          mergePairs h@(T' lh x E') = h
          mergePairs (T' lh1 x (T' lh2 y rh2))
              -- the follows is equal to: merge (merge (T' lh1 x E') (T' lh2 y E')) $ mergePairs rh2
              | x <= y    = merge (T' (T' lh2 y lh1) x E') $ mergePairs rh2
              | otherwise = merge (T' (T' lh1 x lh2) y E') $ mergePairs rh2
