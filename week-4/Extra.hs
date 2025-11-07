{-
1. Sieve of Eratosthenes
Implement the classic Sieve of Eratosthenes to generate all primes up to n.
Focus: filter, map, recursion, and functional style.
-}

helper :: [Integer] -> [Integer]
helper [] = []
helper (x: xs) = x : helper (filter (\a -> a `mod` x /= 0) xs)

sieve :: Integer -> [Integer]
sieve n = helper [2 .. n]

{-
2. Generate Pythagorean Triples
Find all triples (a,b,c) such that a^2 + b^2 = c^2 for numbers up to n.
Focus: Cartesian product (or nested list comprehensions), filtering, and mapping.
-}

generatePythagoreanTriples :: Integer -> [(Integer,Integer,Integer)]
generatePythagoreanTriples 0 = []
generatePythagoreanTriples n =
    let list = [1 .. n]
        triplets = [(a,b,c)| a <- [1..n],b<-[1..n],c<-[1..n], a^2 + b^2 == c^2, a < b , b < c]
    in triplets

{-
3. Pascal’s Triangle
Generate rows of Pascal’s triangle up to n.
Focus: using zipWith or map to compute each row from the previous one.
-}

helper_2 :: [Integer] -> [Integer]
helper_2 xs = zipWith (+) (0:xs) (xs ++ [0])

pascalsTriangle :: Integer -> [[Integer]]
pascalsTriangle 0 = []
pascalsTriangle 1 = [[1]]
pascalsTriangle n =
    let prev = pascalsTriangle (n-1)
        next = helper_2 (last prev)
    in prev ++ [next]
main = do
    print (sieve 20)
    print (generatePythagoreanTriples 20)
    print(pascalsTriangle 5)

