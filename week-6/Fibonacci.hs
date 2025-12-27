fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib1 :: [Integer]
fib1 = map fib [0 .. ]

fib2 :: [Integer]
fib2 = 0 : 1 : zipWith (+) fib2( tail fib2)

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a 
streamRepeat n = Stream n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Stream n(streamFromSeed f (f n ))

nats :: Stream Integer
nats = streamFromSeed (+1) 0
main = do
    print fib2