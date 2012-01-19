module LeftiestHeap (LeftiestHeap(..)) where

import Heap

data LeftiestHeap a = E
                    | T Int a (LeftiestHeap a) (LeftiestHeap a)
                    deriving (Show)

instance Heap LeftiestHeap where
  empty     = E
  isEmpty E = True
  isEmpty _ = False

  {-
  -- Original
  insert    x h         = merge (T 1 x E E) h
  -}

  -- Exercise 3.2
  insert x1 E = T 1 x1 E E
  insert x1 h2@(T r x2 a2 b2)
    | x1 <= x2  = makeT x1 E h2
    | otherwise = makeT x2 a2 $ insert x1 b2

  merge h E = h
  merge E h = h
  merge h1@(T _ x1 a1 b1) h2@(T _ x2 a2 b2) =
    if x1 <= x2 then
      makeT x1 a1 (merge b1 h2)
    else
      makeT x2 a2 (merge h1 b2)

  findMin   (T _ x a b) = x
  deleteMin (T _ x a b) = merge a b

rank E           = 0
rank (T r _ _ _) = r

-- Exercise 3.4 (b)

{-
-- Spine version
makeT x a b =
    if rank a >= rank b then
      T ((rank b) + 1) x a b
    else
      T ((rank a) + 1) x b a
-}

-- Weight version
make T x a b =
    let a_size = rank a
        b_size = rank b
        next_size = a_size + b_size + 1
    in if a_size >= b_size then
           T next_size x a b
       else
           T next_size x b a

-- Exercise 3.3
fromList list = case fl [T 1 x E E | x <- list] of
                  [] -> E
                  r  -> head r
  where fl (x:y:[]) = [merge x y]
        fl (x:y:xs) = fl $ merge x y : fl xs
        fl (x:[])   = [x]
        fl []       = [E]
