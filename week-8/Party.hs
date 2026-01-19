{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Monoid
import Data.Tree
import Employee


-- Add an employee to a guest list (ignores boss/subordinate constraints)
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  GL xs f1 `mappend` GL ys f2 = GL (xs ++ ys) (f1 + f2)

-- Pick the guest list with the greater total fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max


glConsTest :: Bool
glConsTest = and
  [ GL [joe] 3         == glCons joe (GL [] 0)
  , GL [sam, joe] 7    == glCons sam (GL [joe] 3)
  , GL [sue, sam, joe] 8 == glCons sue (GL [sam, joe] 7)
  ]
  where
    joe = Emp "Joe" 3
    sam = Emp "Sam" 4
    sue = Emp "Sue" 1

instanceTest :: Bool
instanceTest = and
  [ GL [] 0        == mempty
  , GL [joe, sam] 7 == glCons joe mempty <> glCons sam mempty
  ]
  where
    joe = Emp "Joe" 3
    sam = Emp "Sam" 4

moreFunTest :: Bool
moreFunTest = and
  [ mempty     == moreFun mempty mempty
  , GL [joe] 7 == moreFun (glCons joe mempty) (glCons joe mempty)
  , GL [joe] 7 == moreFun (glCons joe mempty) (glCons sam mempty)
  ]
  where
    joe = Emp "Joe" 7
    sam = Emp "Sam" 4


-- Fold a tree bottom-up (children first, then the parent)
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x cs) = f x (map (treeFold f) cs)


treeFoldTest :: Bool
treeFoldTest = and
  [ 8  == (treeFold (\_ xs -> 1 + sum xs) testCompany :: Integer)
  , 17 == treeFold (\x xs -> max (empFun x) (myMax xs)) testCompany
  , 46 == treeFold (\x xs -> empFun x + sum xs) testCompany
  ]
  where
    myMax [] = 0
    myMax ys = maximum ys


-- For a boss and results from subtrees, return:
--   (best list including the boss, best list excluding the boss)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subResults = (withBoss, withoutBoss)
  where
    withBoss    = glCons boss (foldMap snd subResults)
    withoutBoss = foldMap fst subResults


nextLevelTest :: Bool
nextLevelTest = (GL [joe] 5, gl) == nextLevel joe [(gl, mempty)]
  where
    joe = Emp "Joe" 5
    sam = Emp "Sam" 2
    sue = Emp "Sue" 2
    gl  = GL [sam, sue] 4


-- Compute the most fun guest list for a company hierarchy
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


maxFunTest :: Bool
maxFunTest = and
  [ GL [joe] 5     == maxFun (Node joe [])
  , GL [sam, sue] 6 == maxFun (Node joe [Node sam [], Node sue []])
  ]
  where
    joe = Emp "Joe" 5
    sam = Emp "Sam" 3
    sue = Emp "Joe" 3


formatEmp :: [Employee] -> String
formatEmp = unlines . sort . map empName

formatGL :: GuestList -> String
formatGL (GL es totalFun) =
  "Total fun: " ++ show totalFun ++ "\n" ++ formatEmp es

main :: IO ()
main = do
  contents <- readFile "company.txt"
  putStr . formatGL . maxFun . read $ contents
