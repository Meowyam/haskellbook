How does fmap.fmap typecheck

(.) :: (b -> c) -> (a -> b) -> a -> c
fmap :: Functor f => (m -> n) -> f m -> f n
fmap :: Functor g => (x -> y) -> g x -> g y

https://stackoverflow.com/questions/23030638/how-fmap-fmap-typechecks

fmap . fmap
functor f. functor g

(.)  :: (a -> b) -> (r -> a) -> (r -> b)
fmap :: Functor f => (c -> d) -> f c -> f d
fmap :: Functor g => (x -> y) -> g x -> g y

(fmap.fmap) :: (Functor f, Functor g) => (x -> y) -> f (g x) -> f (g y)

from stackoverflow:

"fmapping" once lifts the function once. fmapping twice lifts that lifted function...so, a double lift

Put in the language of haskell syntax:

f                    ::         a   ->         b
fmap f               ::       f a   ->       f b
fmap (fmap f)        ::    g (f a)  ->    g (f b)
fmap (fmap (fmap f)) :: h (g (f a)) -> h (g (f b))

f                   ::   a   ->   b   ->   c
zipWith f           ::  [a]  ->  [b]  ->  [c]
zipWith (zipWith f) :: [[a]] -> [[b]] -> [[c]]


etc.


and this is how I understood fmap.fmap.
