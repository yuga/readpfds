{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module ExplicitMin (ExplicitMin) where

    import Heap

    data (Heap h) => ExplicitMin h a = E
                                     | NE a (h a)
                                     deriving (Show)

    instance (Heap h) => Heap (ExplicitMin h) where
      empty = E

      isEmpty E = True
      isEmpty _ = False

      insert x E = NE x $ insert x $ empty
      insert x (NE m h)
          | x <= m    = NE x $ insert x h
          | otherwise = NE m $ insert x h

      merge E e = e
      merge e E = e
      merge (NE m1 h1) (NE m2 h2)
          | m1 < m2   = NE m1 $ merge h1 h2
          | otherwise = NE m2 $ merge h1 h2

      findMin E = error "empty heap"
      findMin (NE m h) = m

      deleteMin E = error "empty heap"
      deleteMin (NE m h) = NE (findMin h') h'
          where h' = deleteMin h
