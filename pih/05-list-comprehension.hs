import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

uplet2int :: Char -> Int
uplet2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2uplet :: Int -> Char
int2uplet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2uplet ((uplet2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: Int -> Int -> Float
percent x y = fromIntegral x / fromIntegral y * 100

--freqs :: String -> [Float]
--freqs xs = [percent x (chr n) | x <- xs, n <- ['a'..'z']]

-- exercises --------------------------------------------------------

sumc :: Int
sumc = sum [x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int,Int)]
grid a b = [(x,y) | x <- [0..a], y <- [0..b]]

square :: Int -> [(Int,Int)]
square a = [(x,y) | (x,y) <- grid a a, x /= y]

-- function replicate using list comprehention
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n]
                   , y <- [1..n]
                   , z <- [1..n]
                   , x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- Returns list of all perfect numbers up to a given limit.
-- A positive integer is perfect if it equals the sum of all
-- of its factors, excluding the number itself.
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
