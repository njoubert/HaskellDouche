import Prelude hiding (return, fail)

data Expr = Num Int | Var String | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
data Statement = Assignment String Expr

-- Parser parses prefix, returns parsed representation and remained string
type Parser a = String -> Maybe (a, String)

char :: Parser Char
char (c:cs) = Just(c,cs)
char [] = Nothing

fail :: Parser a
fail cs = Nothing

return :: a -> Parser a
return a cs = Just(a,cs)

