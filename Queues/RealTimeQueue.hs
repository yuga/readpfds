{-# LANGUAGE BangPatterns #-}

module RealTimeQueue where

----------------------------------------------------------------
import Control.Applicative ((<$>))
import qualified GHC.Vacuum
import qualified GHC.Vacuum.ClosureType
import System.IO.Unsafe

isThunk :: a -> IO Bool
isThunk x = GHC.Vacuum.ClosureType.isThunk <$> GHC.Vacuum.closureType x

newtype LazyList a = Lazy [a]
newtype StrictList a = Strict [a]

showLazy :: Show a => LazyList a -> String
showLazy (Lazy ys) = unsafePerformIO (go ys)
  where
    go xs = do
        thunk <- isThunk xs
        if thunk then
            return "?"
          else
            case xs of
                []    -> return "[]"
                x:xs' -> (\rest -> show x ++ ":" ++ rest) <$> go xs'

showStrict :: Show a => StrictList a -> String
showStrict (Strict ys) = unsafePerformIO (go ys)
  where
    go xs = do
        thunk <- isThunk xs
        if thunk then
            error "showStrict"
          else
            case xs of
                []    -> return "[]"
                x:xs' -> (\rest -> show x ++ ":" ++ rest) <$> go xs'

conS :: a -> StrictList a -> StrictList a
conS x (Strict xs) = Strict xs'
  where
    !xs' = x:xs

infixl 0 >-
(>-) :: a -> (a -> b) -> b
a >- f = f a
----------------------------------------------------------------

data RQ a = RQ  (LazyList a)    -- front
               !(StrictList a)  -- rear
                (LazyList a)    -- pointer copy to front

instance Show a => Show (RQ a) where
    show (RQ fq rq cq) = "RQ " ++ showLazy fq ++ " " ++ showStrict rq ++ " " ++ showLazy cq

rotate :: LazyList a -> StrictList a -> LazyList a -> LazyList a
rotate (Lazy [])     (Strict [y])    (Lazy zs) = Lazy (y:zs)
rotate (Lazy (x:xs)) (Strict (y:ys)) (Lazy zs) =
    let Lazy rs = rotate (Lazy xs) (Strict ys) (Lazy (y:zs)) -- reverse
    in Lazy (x:rs)
rotate _ _ _            = error "rotate"

exec :: RQ a -> RQ a
exec (RQ f r (Lazy (_:x))) = RQ f r (Lazy x)      -- forcing
exec (RQ f r (Lazy  []))   = RQ f' (Strict []) f'
  where
    f' = rotate f r (Lazy [])

enqueue :: a -> RQ a -> RQ a
enqueue x (RQ f r s) = let !z = exec (RQ f r' s) in z
  where
    r' = conS x r

dequeque :: RQ a -> RQ a
dequeque (RQ (Lazy (_:f)) r s) = exec (RQ (Lazy f) r s)
dequeque _                     = error "dequeue"

top :: RQ a -> a
top (RQ (Lazy (x:_)) _ _) = x
top _                     = error "top"

empty :: RQ a
empty = RQ (Lazy []) (Strict []) (Lazy [])
