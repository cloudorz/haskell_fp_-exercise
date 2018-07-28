data Expr
       = Lit Integer
       | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add la lb) = eval la + eval lb

printExpr :: Expr -> String 
printExpr (Lit n) = show n
printExpr (Add la lb) = printExpr la ++ " + " ++ printExpr lb
