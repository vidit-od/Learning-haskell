import Parser (parseExp)
import ExprT
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalString :: String -> Maybe Integer
evalString = parseExp id (+) (*)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr Exp where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (<= 0)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = id
    add = max
    mul = min

instance Expr Mod7 where
    lit = id `mod` 7
    add a b = (a + b ) `mod` 7
    mul a b = (a * b) `mod` 7

main = do
    let a = Lit 5
        b = Lit 7
        c = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    print (evalString "(2+3)*4")