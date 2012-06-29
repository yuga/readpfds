module BatchedQueue (BatchedQueue(..)) where

import Prelude hiding(head,tail)
import Queue

data BatchedQueue a = BQ [a] [a]
                    deriving (Show)

checkf [] r = BQ (reverse r) []
checkf f  r = BQ f r

instance Queue BatchedQueue where
    empty             = BQ [] []
    isEmpty (BQ f r)  = null f

    head (BQ []    _) = error "empty queue"
    head (BQ (x:f) r) = x

    snoc (BQ f r) x   = checkf f (x:r)

    tail (BQ []    _) = error "empty queue"
    tail (BQ (x:f) r) = checkf f r