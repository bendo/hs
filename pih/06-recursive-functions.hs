-- recursive functions

-- 5 steps
-- step 1: define the type
-- step 2: enumerate the cases
-- step 3: define the simple cases
-- step 4: define the other cases
-- step 5: generalise and simplify

--example drop--------------------------------------------------------
{-
-- step 1
drop :: Int -> [a] -> [a]

-- step 2
drop 0 []     =
drop 0 (x:xs) =
drop n []     =
drop n (x:xs) =

-- step 3
drop 0 []     = []
drop 0 (x:xs) = x:xs
drop n []     = []
drop n (x:xs) =

-- step 4
drop 0 []     = []
drop 0 (x:xs) = x:xs
drop n []     = []
drop n (x:xs) = drop (n-1) xs

-- step 5
drop :: Integral b => b -> [a] -> [a]
drop 0 xs     = xs
drop n []     = []
drop n (x:xs) = drop (n-1) xs
-}
--example init--------------------------------------------------------
{-
-- step 1
init :: [a] -> [a]

-- step 2
init (x:xs) =

-- step 3
init (x:xs) | null xs   = []
            | otherwise =

-- step 4
init (x:xs) | null xs   = []
            | otherwise = x : init xs

-- step 5
init :: [a] -> [a]
init [_]    = []
init (x:xs) = x : init xs
-}
----------------------------------------------------------------------

fac :: Int -> Int
fac 0 = 1               -- base case
fac n = n * fac (n-1)   -- recursive case

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

length' :: [a] -> Int
length' [] = 0
length' (_:ns) = 1 + length' ns

-- exercises ---------------------------------------------------------
fac' :: Int -> Int
fac' 0 = 1
fac' n | n > 0 = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n-1)

na :: Int -> Int -> Int
na m 0 = 1
na m n = m * (na m (n-1))

euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a < b = euclid a (b-a)
           | a > b = euclid (a-b) b

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate' (x-1) y

pos :: [a] -> Int -> a
pos (x:_) 0 = x
pos (_:xs) n = pos xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' n (x:xs) = (n == x) || elem' n xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] []         = []
merge (x:xs) (y:ys) = merge sorted (xs ++ ys)
                      where
                        sorted = if x <= y then [x,y] else [y,x]

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b >  x]

msort :: Ord a => [a] -> [a]
msort x = merge left right
          where
            (left,right) = halve x

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve x  = (first x,second x)

first []     = []
first (x:xs) = x : second xs

second []     = []
second (_:xs) = first xs
