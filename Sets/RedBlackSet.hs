{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module RedBlackSet (RedBlackSet) where

import Set

data Color = R | B
data RedBlackSet a = E | T Color (RedBlackSet a) a (RedBlackSet a)

balance B (T R (T R a x b) y c) z d = T R (T R a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b                 = T color a x b

-- https://github.com/shtaag/Haskell/blob/master/pfds/RedBlackSet.hs
-- 参考にした論文: http://t.co/P3SiwhRE
-- http://www.slideshare.net/ShigekazuTakei/pfds-ex3-9
data Digit a = One a (RedBlackSet a)
             | Two a (RedBlackSet a) a (RedBlackSet a)

incr :: Digit a -> [Digit a] -> [Digit a]
incr (One a t) [] = [One a t]
incr (One a1 t1) (One a2 t2 : ps) = Two a1 t1 a2 t2 : ps
incr (One a1 t1) (Two a2 t2 a3 t3 : ps) = One a1 t1 : incr (One a2 (T B t2 a3 t3)) ps

bottom_up :: [a] -> RedBlackSet a
bottom_up = linkAll . foldr add []
  where
    add a ps = incr (One a E) ps
    linkAll = foldl link E
    link l (One a t) = T B l a t
    link l (Two a1 t1 a2 t2) = T B (T R l a1 t1) a2 t2

fromOrdList :: Ord a => [a] -> RedBlackSet a
fromOrdList = bottom_up


instance (Ord a) => Set RedBlackSet a where
    empty = E

    member x E = False
    member x (T _ a y b)
      | x < y = member x a
      | x > y = member x b
      | otherwise = True

    insert x s = T B a y b
      where
        ins E = T R E x E
        ins s@(T color a y b)
          | x < y = balance color (ins a) y b
          | x > y = balance color a y (ins b)
          | otherwise = s
        T _ a y b = ins s -- guranteed to be non-empty
