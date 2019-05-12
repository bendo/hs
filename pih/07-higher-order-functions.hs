import           Data.Char

twice :: (a -> a) -> a -> a
twice f x = f (f x)

twice' = twice (+2)

-- map :: (a -> b) -> [a] -> [b]
-- map f xs = [f x | x <- xs]
--
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p xs = [x | x <- xs, p x]
--
-- foldr
-- f []     = v
-- f (x:xs) = x # f xs
--
-- sum []     = 0
-- sum (x:xs) = x + sum xs
--
-- sum :: Num a => [a] -> a
-- sum = foldr (+) 0
--
-- 1 : (2 : (3 : []))
-- 1 + (2 + (3 + 0))
--
-- foldr (#) v [x0,x1,...,xn] = x0 # (x1 # (... (xn # v) ...))
--
-- foldl
-- f v []     = v
-- f v (x:xs) = f (v # x) xs
--
-- sum :: Num a => [a] -> a
-- sum = foldl (+) 0
--
-- foldl (#) v [x0,x1,...,xn] = (... ((v # x0) # x1) ...) # xn

-- excercises ----------------------------------------------------------------

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []     = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x,y) -> f x y

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Int]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

chop8' :: Eq a => [a] -> [[a]]
chop8' = unfold (== []) (take 8) (drop 8)

map'' :: Eq a => (a -> b) -> [a] -> [b]
map'' f = unfold (== []) (f . head) tail

--iterate' :: Eq a => (a -> a) -> a -> [a]
--iterate' f = unfold (== []) f f

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []     = []
altMap f g (x:xs) = f x : altMap g f xs

luhnDouble :: Int -> Int
luhnDouble x = if 2 * x > 9 then 2 * x - 9 else 2 * x

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble (*1) xs) `mod` 10 == 0

-- Binary string transmitter

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> [Bit]
parity bits | isparityone bits = bits ++ [1]
            | otherwise        = bits ++ [0]

isparityone :: [Bit] -> Bool
isparityone = odd . length . filter (==1)

encode :: String -> [Bit]
encode = concat . map (parity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . checkparity) . chop9

checkparity :: [Bit] -> [Bit]
checkparity bits | chck8 && last bits == 1     = chop
                 | not chck8 && last bits == 0 = chop
                 | otherwise                  = error "Parity check failed"
                  where
                    chck8 = isparityone $ init bits
                    chop  = init bits

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id -- use tail if you wonna see how it brakes
