-- Exercise 3.6
module BinomialHeap2 (BinomialHeap2) where
    import Heap

    data T a = Node a [T a]
             deriving (Show)
    type Tree a = (Int, T a)
    newtype BinomialHeap2 a = BH [Tree a]
                              deriving (Show)

    link :: (Ord a) => Tree a -> Tree a -> Tree a
    link (r, t1@(Node x1 c1)) (_, t2@(Node x2 c2))
        | x1 <= x2  = (r + 1, Node x1 (t2:c1))
        | otherwise = (r + 1, Node x2 (t1:c2))

    rank :: Tree a -> Int
    rank (r, _) = r

    root :: Tree a -> a
    root (_, Node x _) = x

    insTree :: (Ord a) => Tree a -> [Tree a] -> [Tree a]
    insTree t []           = [t]
    insTree t ts@(t':ts')
        | rank t < rank t' = t:ts
        | otherwise        = insTree (link t t') ts'

    mrg :: (Ord a) => [Tree a] -> [Tree a] -> [Tree a]
    mrg ts1 []  = ts1
    mrg []  ts2 = ts2
    mrg ts1@(t1:ts1') ts2@(t2:ts2')
        | rank t1 < rank t2 = t1 : mrg ts1' ts2
        | rank t2 < rank t1 = t2 : mrg ts1  ts2'
        | otherwise         = insTree (link t1 t2) (mrg ts1' ts2')

    removeMinTree :: (Ord a) => [Tree a] -> (Tree a, [Tree a])
    removeMinTree []     = error "empty heap"
    removeMinTree [t]    = (t,[])
    removeMinTree (t:ts) =
        if root t < root t' then
            (t,ts)
        else
            (t', t:ts')
        where (t', ts') = removeMinTree ts

    instance Heap BinomialHeap2 where
        empty = BH []

        isEmpty (BH ts) = null ts

        insert x (BH ts) = BH (insTree (0, Node x []) ts)

        merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)

        findMin (BH ts) = minimum $ fmap root ts

        deleteMin (BH ts) = BH (mrg (zip [1..] $ reverse ts1)  ts2)
            where ((r, Node x ts1), ts2) = removeMinTree ts

