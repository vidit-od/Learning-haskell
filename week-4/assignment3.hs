{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
xor :: [Bool] -> Bool
xor = foldr (/=) False  . filter id

{-
foldr (/=) False [True, True]
→ True /= (True /= False)
→ True /= True
→ False
-}

map' :: (a->b) ->[a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- ([] ++ f a[0]) ++

flipper :: Bool -> Bool
flipper x
    | x = False
    | otherwise = True

myfoldl :: (a->b->a) -> a -> [b] -> a
myfoldl f base xs = foldr (\x g acc -> g (f acc x)) id xs base

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]



sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    let allPairs = cartesianProduct [1 .. n] [1 .. n]
        filteredPairs = filter (\(i,j) -> i + j + 2*i*j <= n && i <= j) allPairs
        numbersToRemove = map (\(i,j) -> i + j + 2*i*j) filteredPairs
        survivors = filter (`notElem` numbersToRemove) [1 .. n]
        prime = map (\x -> 2*x + 1) survivors
        result = 2 : prime
    in  result



main = do
    let list = [True, True, False, False,True]
    print ( xor list)
    print ( map' flipper list)
    print ( sieveSundaram 10)
    print "done"
