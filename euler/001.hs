{- 
 - If we list all the natural numbers below 10 that are multiples
 - of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 -
 - Find the sum of all the multiples of 3 or 5 below 1000.
 -}

module Main where

import Data.List (union)

divBy3 :: [Integer]
divBy3 = [x | x <- [1..999], x `mod` 3 == 0]

divBy5 :: [Integer]
divBy5 = [x | x <- [1..999], x `mod` 5 == 0]

main :: IO ()
main = putStrLn $ show $ sum $ union divBy3 divBy5

