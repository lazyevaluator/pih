import Control.Applicative
import Data.Char
{-
  A parser for things
  Is a function from strings
  To lists of pairs
  Of things and strings
-}
--13.2
newtype Parser a = P (String -> [(a, String)])

--13.3
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p 

item :: Parser Char
item = P (\inp -> case inp of
                      []     -> []
                      (x:xs) -> [(x,xs)])


--13.4
instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                            [] -> []
                            [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> [(x, inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                            []         -> []
                            [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                        []         -> []
                        [(v, out)] -> parse (f v) out)

three :: Parser (Char, Char)
three = do x <- item
           item
           z <- item
           return (x, z)

--13.5
instance Alternative Parser where
  --empty :: Parser a
  empty = P $ const []

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                        []        -> parse q inp
                        [(v,out)] -> [(v, out)])


--13.6
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|> nat

--13.7
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

--13.8
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
  [(n, [])]  -> n
  [(_, out)] -> error ("Unused input" ++ out)
  []         -> error "Invalid input"