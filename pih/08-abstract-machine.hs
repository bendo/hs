data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val x)   = x
value (Add x y) = value x + value y


