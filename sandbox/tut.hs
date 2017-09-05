import Data.List
import System.IO

-- funcName param1 param2 = operations (returned value)

-- touples
addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)


-- what age
whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You are adult"
whatAge _ = "Nothing important"

-- factiorial -> recursion  
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- another solution for factorial
prodFact n = product [1..n]


-- is odd
isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True

isEven n = n `mod` 2 == 0


-- what Grade
whatGrade :: Int -> String
whatGrade age
    | (age >= 5) && (age <= 6) = "Kindergarten"
    | (age > 6) && (age <= 10) = "Elementary School"
    | (age > 10) && (age <= 14) = "Middle School"
    | (age > 14) && (age <= 18) = "High School"
    | otherwise = "Go to college"

-- batAvgRating -> where
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
    | avg <= 0.200 = "Terrible Batting Average"
    | avg <= 0.250 = "Average Player"
    | avg <= 0.280 = "You doing pretty good"
    | otherwise = "You're a Superstar"
    where avg = hits / atBats

-- get List items
getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems [x] = "Your list starts with " ++ show x
getListItems [x:y] = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "Your list starts with " ++ show x ++ " and contains " ++ show xs


-- getFirstItem
getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]



{- Higher order functions -}

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs


-- compare two strings
areStrEq :: [Char] -> [Char] -> Bool
areStrEq [] [] = True
areStrEq (x:xs) (y:ys) = x == y && areStrEq xs ys
areStrEq _ _ = False

--pass function to the function
doMult :: (Int -> Int) -> Int
doMult func = func 3

num3Times4 = doMult times4


