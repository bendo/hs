module Fn
where


maxi :: Ord a => a -> a -> a
maxi x y = if x >= y then x else y 


signum :: (Ord a, Num a) => a -> Int
signum x | x <  0    = -1
         | x == 0    = 0
         | otherwise = 1

type Color = String

type ColorPoint = (Int, Int, Color)

origin :: Color -> ColorPoint
origin color = (0, 0, color)

move :: ColorPoint -> Int -> Int -> ColorPoint
move (x, y, color) dx dy = (x + dx, y + dy, color) 

startPoint = (0,0,"black")

colorOfPoint (x,y,color) = color

oddNumbers :: Int -> [Int]
oddNumbers maxNum = [1,3..maxNum]

sort2 :: Ord a => a -> a -> (a, a)
sort2 x y | x < y = (x, y)
          | otherwise = (y, x)

almostEqual (x1, y1) (x2, y2)
    | x1 == x2 = y1 == y2
    | x1 == y2 = x2 == y1
    | otherwise = False

almostEqual' pair1 pair2 = (pair1 == pair2) || (swap pair1 == swap pair2)
    where
        swap (x,y) = (y,x)

isLower :: Char -> Bool
isLower x 
    | x `elem` ['a'..'z'] = True
    | otherwise = False

mangle :: String -> String
mangle [] = ""
mangle (x:xs) = xs ++ [x]

biggest, smallest :: Int
biggest = maxBound
smallest = minBound

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

numDigs :: Int
numDigs = length (show reallyBig)

d1, d2 :: Double
d1 = 4.3223
d2 = 6.323e-5

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

unicorn :: Integer -> Integer
unicorn n
    | mod n 2 == 0 = div n 2
    | otherwise    = 3 * n + 1

unicorns :: Integer -> [Integer]
unicorns 1 = [1]
unicorns n = n : unicorns (unicorn n)

foo :: Int -> Int
foo 0 = 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise         = 4
foo n
    | n < 0             = 0
    | mod n 17 == 2     = -43
    | otherwise         = n + 3

isEven :: Integer -> Bool
isEven n = mod n 2 == 0

pa :: (Int, Char)
pa = (3, 'a')

sumLa :: (Int, Int) -> Int
sumLa (x, y) = x + y

data BookInfo     = Book Int String [String] deriving (Show)
data MagazineInfo = Magazine Int String [String] deriving (Show)

myBook = Book 1 "Harry Potter" ["R.W. Rowling", "John Johnson"]

---------------------------------------------------------------
data FailableDouble = Failed
                    | Ok Double
                    deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failed
safeDiv x y = Ok (x/y)

failureToZero :: FailableDouble -> Double
failureToZero Failed = 0
failureToZero (Ok x) = x
---------------------------------------------------------------
data Thing = Shoe
           | Pencil
           | Sword
           | Calculator
           | Unicorn
           deriving Show

type Name = String
type Age = Int
type FavoritThing = Thing

data Person = Person Name Age FavoritThing deriving Show

tom :: Person
tom = Person "Tom Cruise" 55 Sword

yvonne :: Person
yvonne = Person "Yvonne Strahowski" 36 Unicorn

getAge :: Person -> Age
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name for " ++ show p ++ " is " ++ n
---------------------------------------------------------------
data IntList = Empty | Lala Int IntList
    deriving Show

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Lala x l) = x * intListProd l
-- intListProd (Lala 2 (Lala 3 (Lala 5 (Empty))))
-- 30
---------------------------------------------------------------

