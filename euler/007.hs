{-
 - By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
 - we can see that the 6th prime is 13.
 -
 - What is the 10 001st prime number?
 -}

module Main where

isPrime :: Integer -> Bool
isPrime x = foo 2
  where
    foo y
      | y * y > x      = True
      | x `mod` y == 0 = False
      | otherwise      = foo (y + 1)

main :: IO ()
main = putStrLn $ show $ filter isPrime [2..1000000] !! 10000

