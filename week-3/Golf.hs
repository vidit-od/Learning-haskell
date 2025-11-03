module Golf where

helper :: Int -> [a] -> [a]
helper _ [] = []
helper n xs
    | length xs < n = []
    | otherwise     = xs !! (n-1) : helper n (drop n xs)


mapperFunc :: [Int] -> (Int -> [a] -> [a]) -> [a] -> [[a]]
mapperFunc _ f [] = []
mapperFunc xs f x = map (`f` x) xs

localMaximum :: [Int] -> [Int]
localMaximum [] = []
localMaximum [x] = []
localMaximum [x,y] = []
localMaximum (a:b:c:rest)
    | (b > a) && (b > c) = b : localMaximum (c:rest)
    | otherwise         = localMaximum (b:c:rest)

skips :: [a] -> [[a]]
skips [] = []
skips xs = mapperFunc [1 .. length xs] helper xs

increment :: Integer -> [Integer] -> [Integer]
increment n x = take (fromInteger n) x ++ [x !! (fromInteger n) + 1] ++ drop ((fromInteger n)+1) x

buildRow :: Integer -> [Integer] -> String
buildRow 0 _ = "==========\n0123456789" 
buildRow _ [] = ""
buildRow n (x:xs)
    | x >= n = "x" ++ buildRow n xs
    | otherwise = " " ++ buildRow n xs

height :: [Integer] -> Integer
height [] = 0
height (x:xs) = max x (height xs)

count :: [Integer] -> [Integer]
count [] = [0,0,0,0,0,0,0,0,0,0]
count (x:xs) = increment x (count xs)

histogram :: [Integer] -> String
histogram [] = ""
histogram x = unlines(map(\h -> buildRow h (count x)) [height (count x), height (count x) - 1 .. 0])



main = do
    let list = [1,1,1,1,1,1,2,3,4,5,6,7]
    putStr (histogram list)

    print ("done")