-- question 1 
-- a
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- wholemeal a
fun1' :: [Integer] -> Integer
fun1' =  foldl (*) 1 . map(\ x -> x - 2) . filter(even)

-- b
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2( n `div` 2)
    | otherwise = fun2(3 *n + 1)

-- wholemeal b
fun2Helper :: Integer -> Integer
fun2Helper 1 = 0
fun2Helper n 
    | even n = n `div` 2
    | otherwise = 3*n + 1

fun2' :: Integer -> Integer
fun2' n = foldl (+) 0 (filter even (takeWhile (>0) (iterate fun2Helper n)))

main = do
    print( fun2' 10)
    print( fun2 10)
    print("")
