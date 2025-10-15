type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n from to aux = 
    hanoi (n-1) from aux to ++ [(from, to)] ++ hanoi (n-1) aux to from

main :: IO()
main = mapM_ print(hanoi 4 "A" "B" "C");