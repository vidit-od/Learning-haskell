import Data.Tree (flatten)
data Tree a = Empty
            | Node (Tree a) a (Tree a)

treeFold :: b ->( b-> a-> b -> b) -> Tree a -> b 
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize' = treeFold 0 (\l _ r -> 1 + l + r)
treeSum' = treeFold 0 (\l x r -> l + x + r)
treeFlatten' = treeFold [] (\l x r -> l ++ [x] ++ r)