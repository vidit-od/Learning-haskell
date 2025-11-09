{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

import StackVM
import ExprT (ExprT)
import Parser (parseExp)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr Program where
    lit n = [StackVM.PushI n]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

run :: String -> Either String StackVal
run = execute . compile
    where execute Nothing = Left "No Compile"
          execute (Just p) = stackVM p

compileTest :: Bool
compileTest = and
  [
    Nothing ==  compile "5+*2",
    Just [PushI 5] == compile "5",
    Just [PushI 5,PushI 6,StackVM.Add] == compile "5+6",
    Just [PushI 5,PushI 6,StackVM.Mul] == compile "5*6",
    Just [PushI 4,PushI 2,PushI 3,StackVM.Mul,StackVM.Add] == compile "4+2*3",
    Just [PushI 4,PushI 2,StackVM.Add,PushI 3,StackVM.Mul] == compile "(4+2)*3"
  ]

runTest :: Bool
runTest = and
  [
    Left "The program does not compile." == run "5+*2",
    Right (IVal 5) == run "5",
    Right (IVal 11) == run "5+6",
    Right (IVal 30) == run "5*6",
    Right (IVal 10) == run "4+2*3",
    Right (IVal 18) == run "(4+2)*3"
  ]

main :: IO()
main = do 
    print (compileTest)
    print (runTest)
