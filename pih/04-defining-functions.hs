module Fun
where

third :: [a] -> a
third xs = head $ tail $ tail xs

third' :: [a] -> a
third' = (!!2)

third'' :: [a] -> a
third'' (_:_:x:_) = x

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
    | null xs   = []
    | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

luhnDouble :: Int -> Int
luhnDouble x = if 2 * x > 9 then 2 * x - 9 else 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum [luhnDouble a, b, luhnDouble c, d] `mod` 10 == 0

mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

main :: IO ()
main = putStr $ unlines $ fizbuzz [1..100]

sum'' []   = 0
sum'' (n:ns) = n + sum'' ns

add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

add2 = add 2

add' :: Num a => t2 -> t1 -> t -> a -> (a -> (a -> a))
add' x y z = \x -> (\y -> (\z -> x + y + z))

add'' :: Num a => a -> (a -> (a -> a))
add'' = \x -> (\y -> (\z -> x + y + z))

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
    where n = div (length xs) 2

-- playing around

test :: Int -> Int
test i | i < 0     = -1
       | i == 0    = 0
       | otherwise = 1

fizbuzz' :: (Integral a, Show a) => a -> String
fizbuzz' i
    | mod i 15 == 0 = "FizzBuzz"
    | mod i 5 == 0  = "Fizz"
    | mod i 3 == 0  = "Buzz"
    | otherwise     = show i

fizbuzz :: [Integer] -> [String]
fizbuzz = map fizbuzz'
