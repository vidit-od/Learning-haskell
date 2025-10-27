data IntList = Empty | ConstructIntList Int IntList
    deriving Show

-- Map Generalization

absAll :: Int -> Int
absAll x 
    | x>= 0 = x
    | otherwise = -x

addOne :: Int -> Int
addOne x = x+1;

squareEach :: Int -> Int
squareEach x = x*x

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (ConstructIntList x xs) = ConstructIntList(f x) ( mapIntList f xs)

-- Filter Generalization
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (ConstructIntList x xs)
    | even x = ConstructIntList x (keepOnlyEven xs)
    | otherwise = keepOnlyEven xs 

checkEven :: Int -> Bool
checkEven x 
    | x `mod` 2 == 0 = True
    | otherwise = False

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList f (ConstructIntList x xs)  
    | f x = ConstructIntList x ( filterIntList f xs)
    | otherwise = filterIntList f xs

-- polymorphic Data types
data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 ( C 4 ( C 5 E))

lst2 :: List Bool
lst2 = C True ( C False E)

-- polymorphic Functions

main = do 
    let exampleList = ConstructIntList (-1) (ConstructIntList 2 ( ConstructIntList (-6) Empty))
    print (mapIntList absAll exampleList)
    print (mapIntList addOne exampleList)
    print (mapIntList squareEach exampleList)
    print (keepOnlyEven exampleList)
    print()