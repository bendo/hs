data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving (Eq, Ord, Show, Read)

t = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))

balanced :: Tree a -> Bool
balanced (Leaf _)   = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

leaves :: Tree a -> Int
leaves (Leaf _)   = 1
leaves (Node l r) = leaves l + leaves r

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs  = Node (balance f) (balance s)
              where (f,s) = halve xs

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

data Expr = Val Int | Add Expr Expr

--folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n)   = f n
folde f g (Add x y) = g x y
