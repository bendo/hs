-- type declarations
type String    = [Char]
type Pos       = (Int, Int)
type Trans     = Pos -> Pos
type Pair a    = (a,a)
type Assoc k v = [(k,v)]

-- data declarations
--data Bool = True | False
data Move = Nord | South | East | West
    deriving Show

move :: Move -> Pos -> Pos
move Nord  (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East  (x,y) = (x+1,y)
move West  (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p     = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev Nord  = South
rev South = Nord
rev East  = West
rev West  = East

data Shape = Circle Float | Rect Float Float
--data Maybe a = Nothing | Just a

savediv :: Int -> Int -> Maybe Int
savediv _ 0 = Nothing
savediv x y = Just (x `div` y)

savehead :: [a] -> Maybe a
savehead [] = Nothing
savehead xs = Just (head xs)

-- newtype declaration
-- if a new type has a single constructor with a single argument
-- newtype improve type safety, without affecting performance
newtype Natur = N Int

-- recursive types

data Nat = Zero | Succ Nat
    deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add' :: Nat -> Nat -> Nat
add' Zero n     = n
add' (Succ m) n = Succ (add' m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
              deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Node (Leaf 6) 7 (Leaf 8)) 9 (Leaf 10))

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)     = x == y
occurs' x (Node l y r) | x == y    = True
                       | x < y     = occurs' x l
                       | otherwise = occurs' x r

data Things = Friendship | Sex | Love
              deriving (Eq, Ord, Show, Read)
