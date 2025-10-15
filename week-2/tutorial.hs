data FailableDouble = Failure
                    | OK Double
    deriving Show

exo1 = Failure
exo2 = OK (2/3)

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)

data Courses = CSE
             | MECH
             | CHEM 
             | PHY
    deriving Show

data Student = Student String Integer Courses
    deriving Show

vidit :: Student
vidit = Student "vidit" 23 CSE

getAge :: Student -> Integer
getAge(Student _ n _) = n

printStudentDetails :: Student -> String
printStudentDetails p@( Student n _ _ ) = 
    "The details of student named " ++ n ++ " is (" ++ show p ++ ")"

main = do
    putStrLn( printStudentDetails vidit)