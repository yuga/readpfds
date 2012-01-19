module PairingHeap (PairingHeap) where

import Heap

data PairingHeap a = E
                   | T a [PairingHeap a]
                   deriving (Show)

mergePairs []  = E
mergePairs [h] = h
mergePairs (h1:h2:hs) =
    merge (merge h1 h2) $ mergePairs hs

instance Heap PairingHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    insert x h = merge (T x []) h

    merge h1 E  = h1
    merge E  h2 = h2
    merge h1@(T x hs1) h2@(T y hs2)
        | x <= y    = T x (h2:hs1)
        | otherwise = T y (h1:hs2)

    findMin E = error "empty heap"
    findMin (T x _) = x

    deleteMin E = error "empty heap"
    deleteMin (T x hs) = mergePairs hs