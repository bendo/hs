module One where

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits(x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev(x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : y * 2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concat $ map toDigits xs

validate :: Integer -> Bool
validate x = mod (sumDigits $ doubleEveryOther $ toDigitsRev x) 10 == 0
