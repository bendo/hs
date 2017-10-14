{- 
 - The prime factors of 13195 are 5, 7, 13 and 29.
 -
 - What is the largest prime factor of the number 600851475143 ?
 -}

module Main where

isDiv :: Integer -> Integer -> Bool 
isDiv n d = mod n d == 0

isPrime :: Integer -> Bool
isPrime x = foo 2
  where
    foo y
      | y * y > x      = True
      | x `mod` y == 0 = False
      | otherwise      = foo (y + 1)

firstPrime :: Integer -> Integer
firstPrime x = ceiling $ sqrt $ fromIntegral x

main :: IO ()
main = putStrLn $ show $ maximum $ filter isPrime $ filter (isDiv 600851475143)
    [(firstPrime 600851475143),(firstPrime 600851475143)-1..1]

