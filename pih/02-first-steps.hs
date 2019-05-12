factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c where {b = 1; c = 2}; d = a * 2

sum' []     = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

last' xs = head $ reverse xs

init' xs = take (length xs - 1) xs

init'' xs = reverse $ tail $ reverse xs
