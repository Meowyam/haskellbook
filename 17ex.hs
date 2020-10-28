import System.IO

-- Type []

one :: a -> [a]
one = pure

oneapp :: [(a -> b)] -> [a] -> [b]
oneapp = (<*>)

-- type IO
two :: a -> IO a
two = pure

twoapp :: IO (a -> b) -> IO a -> IO b
twoapp = (<*>)

-- type (,) a
three :: Monoid a => b -> (a, b)
three = pure

threeapp :: Monoid a => (a, (b -> c)) -> (a, b) -> (a, c)
threeapp = (<*>)

-- type (->) e
four :: a -> (e -> a)
four = pure

fourapp :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
fourapp = (<*>)
