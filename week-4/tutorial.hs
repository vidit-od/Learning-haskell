
grt100 :: Integer -> Bool
grt100 n
    | n > 100 = True
    | otherwise = False 

greaterThan100_1 :: [Integer] -> [Integer]
greaterThan100_1 [] = []
greaterThan100_1 (x:xs) 
    | x > 100 =  [x] ++ greaterThan100_1 xs
    | otherwise = greaterThan100_1 xs


greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 x = filter (\x -> x > 100) x

greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 x = filter (>100) x

foobar :: [ Integer] -> Integer
foobar [] = 0
foobar (x:xs) 
    | x > 3 = (7*x + 2) + foobar xs
    | otherwise = foobar xs

foobar_2 :: [Integer] -> Integer
foobar_2 = sum . map (\x -> 7*x + 2) . filter(>3)

-- rewriting common functions with fold
fold :: b -> (a->b->b) -> [a] -> b
fold z f [] = z
fold z f (x:xs) = f x (fold z f xs)

sum'' :: [Integer] -> Integer
sum'' = fold 0 (+)

product'' :: [Integer] -> Integer
product'' = fold 1 (*)

length'' :: [Integer] -> Integer
length'' = fold 0 (\_ s -> 1 + s)

and'' :: [Bool] -> Bool
and'' = fold True (&&)

or'' :: [Bool] -> Bool
or'' = fold False (||)

any'' :: (a-> Bool) -> [a] -> Bool
any'' f = fold False (\x y -> f x || y)

all'' :: (a->Bool) -> [a] -> Bool
all'' f = fold True (\x y -> f x && y)

main = do 
    let list = [1,2,3,4,5,100,101]
    print("Return")