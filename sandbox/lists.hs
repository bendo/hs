import Data.List
import System.IO

primeNumbers = [3,5,7,11]

morePrime = primeNumbers ++ [13,17,19,23,29]

favNums = 2 : 7 : 21 : []

multiLists = [[1,2,3],[4,5,6],[7,8,9]]

morePrime2 = 2 : morePrime

lenPrimes = length morePrime2

revPrimes = reverse morePrime2

isListEmpty = null morePrime2

secondPrime = morePrime2 !! 1

firstPrime = head morePrime2

lastPrime = last morePrime2

primeInit = init morePrime2

first3Primes = take 3 morePrime2

removedPrimes = drop 3 morePrime2

is7InList = 7 `elem` morePrime2

is7InList2 = elem 7 morePrime2

maxPrime = maximum morePrime2

minPrime = minimum morePrime2

newList = [2,2,3,5]
-- product je cislo ktore je delitelne vsetkymi cislami v liste
prodPrimes = product newList

zeroToTen = [0..10]

evenList = [2,4..20]

letterList = ['A','C'..'Z']

infList = [10,20..]

many2s = take 10 (repeat 2)

many3s = replicate 10 3

cycleList = take 15 (cycle [1,2,3,4,5])

listTimes2 = [x * 2 | x <- [1..10]]

listTimes3 = [x * 3 | x <- [1..10], x * 10 <= 50]
-- list cisel delitelnych 13timi a 9timi od 0 do 500 
dividBy13And9 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [4,5,2,19,1,8,20]

sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

mulOfLists = zipWith (*) [1,2,3,4,5,1,2] [6,7,8,9,10,11]

listBiggerThan5 = filter (>5) morePrime

evensUpTo20 = takeWhile (<= 20) [2,4..]

-- 15 -> 1 + 2 + 3 + 4 + 5
sumOfList = foldl (+) 1 [2,3,4,5]

-- 120 -> 1 * 2 * 3 * 4 * 5
mulOfList = foldl (*) 1 [2,3,4,5]

-- 120 -> 1 * 5 * 4 * 3 * 2
mulOfListR = foldr (*) 1 [2,3,4,5]

pow3List = [3^n | n <- [1..10]]

multTable = [[x * y | y <- [1..10]] | x <- [1..10]] 

