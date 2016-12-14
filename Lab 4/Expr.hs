module Expr where

import Parsing
import Data.Maybe
import Data.Char
import Test.QuickCheck

data Expr = Num Double
          | Var Char
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          deriving (Show, Eq)

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Var c)     = "x"
showExpr (Add e1 e2) = (showExpr e1) ++ "+" ++ (showExpr e2)
showExpr (Mul e1 e2) = (showFactor e1) ++ "*" ++ (showFactor e2)
showExpr (Sin e)     = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e)     = "cos(" ++ showExpr e ++ ")"


showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

eval :: Expr -> Double -> Double
eval (Num n) x     = n
eval (Var c) x     = x
eval (Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Sin e) x     = sin (eval e x)
eval (Cos e) x     = cos (eval e x)

readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                Just (e,"") -> Just e
                _           -> Nothing

num :: Parser Expr
num = Num <$> readsP

expr = foldr1 Add <$> chain term (char '+')

term = foldr1 Mul <$> chain factor (char '*')

sinFunc = Sin <$> ((char 's') *> (char 'i') *> (char 'n') *> factor) 

cosFunc = Cos <$> ((char 'c') *> (char 'o') *> (char 's') *> factor) 

var = Var <$> (char 'x')

factor = char '(' *> expr <* char ')' <|> num <|> 
         sinFunc <|> cosFunc <|> var


simplify :: Expr -> Expr
simplify e = case e of 
        (Add (Num 0) e)         -> simplify e
        (Add e (Num 0))         -> simplify e
        (Mul (Num 0) _)         -> Num 0
        (Mul _ (Num 0))         -> Num 0
        (Mul (Num 1) e)         -> simplify e
        (Mul e (Num 1))         -> simplify e
        (Add (Num n) (Num m))   -> Num (n+m)
        (Add e1 e2)             -> (Add (simplify e1) (simplify e2))
        (Mul (Num n) (Num m))   -> Num (n*m)
        (Mul e1 e2)             -> (Mul (simplify e1) (simplify e2))
        (Sin e)                 -> (Sin (simplify e))
        (Cos e)                 -> (Cos (simplify e))    
        otherwise               -> e



differentiate :: Expr -> Expr
differentiate e = case e of
    (Num n)           -> Num 0
    (Var x)           -> Num 1
    (Add e1 e2)       -> (Add (differentiate e1) (differentiate e2))
    (Mul e1 e2)       -> (Add (Mul e1 (differentiate e2)) 
                              (Mul (differentiate e1) e2))
    (Sin e)           -> (Cos e)
    (Cos e)           -> (Mul (Num (-1)) (Sin e))


