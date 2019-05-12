data Nat = Zero | Succ Nat
           deriving Show

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero     = Zero
mult m (Succ n) = add m (mult m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
              deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 2 (Leaf 3)) 5 (Node (Leaf 6) 7 (Leaf 8))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                            LT -> occurs x l
                            EQ -> True
                            GT -> occurs x r
