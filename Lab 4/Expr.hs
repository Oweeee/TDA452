import Parsing

data Expr = Num Double
          | Var   
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr Var         = "x"
showExpr (Add e1 e2) = (showExpr e1) ++ " + " ++ (showExpr e2)
showExpr (Mul e1 e2) = (showFactor e1) ++ " * " ++ (showFactor e2)
showExpr (Sin e)     = "sin (" ++ showExpr e ++ ")"
showExpr (Cos e)     = "cos (" ++ showExpr e ++ ")"


showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

eval :: Expr -> Double -> Double
eval (Num n) _     = n
eval Var n         = n
eval (Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Sin e) x     = sin (eval e x)
eval (Cos e) x     = cos (eval e x)

readExpr :: String -> Maybe Expr
readExpr s = 


