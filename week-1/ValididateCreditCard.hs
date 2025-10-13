{-
Question : 
In this section, you will implement the validation algorithm for
credit cards. It follows these steps:

• Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6].

• Add the digits of the doubled values and the undoubled digits from the original number. For example, [2,3,16,6] becomes
2+3+1+6+6 = 18.

Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid
-}


-- approach : 

{-
- ask for user input 
- convert to array 
- double odd index 
- add all 
-}

import Text.Read (readMaybe)

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = toDigits(n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigits(n `div` 10) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther xs = doubleEveryOther (init (init xs)) ++ [ 2 * last(init xs), last xs]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits xs = sumDigits(init xs) + last xs

validate :: Integer -> Bool
validate n 
    | n `mod` 8 == 0 = True
    | otherwise      = False


main = do 
    putStrLn "Enter a credit card number:"
    input <- getLine
    case readMaybe input :: Maybe Integer of
        Nothing -> putStrLn "Invalid input"
        Just num -> do 
            print (validate(sumDigits(doubleEveryOther (toDigits num))))