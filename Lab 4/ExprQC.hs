module ExprQC where

import Expr
import Test.QuickCheck

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = let s = showExpr e
                          Just e' = readExpr s
                      in showExpr e' == s

prop_Simplify :: Expr -> Double -> Bool
prop_Simplify e x=   (abs ((eval e x) - (eval (simplify e) x))) < 0.0001 

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1, rNum), (1, rVar), (s, rOp s), (s, rFunc s)]
    where
        rNum = elements $ map Num [0.0..100.0]
        rVar = elements $ map Var ['x'] 
        rOp s = do
            let s' = (div s 2)
            op <- elements [Mul, Add]
            e1 <- arbExpr s'
            e2 <- arbExpr s'
            return $ op e1 e2
        rFunc s = do
            let s' = (div s 2)
            func <- elements [Sin, Cos]
            e <- arbExpr s'
            return $ func e

instance Arbitrary Expr where
    arbitrary = sized arbExpr
