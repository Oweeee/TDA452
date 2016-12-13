import Parsing
import Data.Maybe
import Data.Char

data Expr = Num Double
          | Var Char
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          deriving (Show)

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

double :: Parser Double
double = (oneOrMore digit >>= return . read) <|> 
         fmap negate (char '-' *> double)

num :: Parser Expr
num = Num <$> double

expr = foldr1 Add <$> chain term (char '+')

term = foldr1 Mul <$> chain factor (char '*')

sinFunc = Sin <$> ((char 's') *> (char 'i') *> (char 'n') *> factor) 

cosFunc = Cos <$> ((char 'c') *> (char 'o') *> (char 's') *> factor) 

var = Var <$> (char 'x')

factor = char '(' *> expr <* char ')' <|> num <|> 
         sinFunc <|> cosFunc <|> var




