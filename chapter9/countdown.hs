--solutions of countdown problem

{-
  Given a sequence of numbers and a target number, attempt to construct
  an expression whose value is the target, by combining one or more numbers 
  from the sequence using addtion, subtraction, multiplication, division 
  and parentheses.
  Numbers in the sequence are positive integers and each number can only be used once.
-}

data Op = Add | Sub | Mul | Div
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

--is the operation valid (in the sense that it is a well defined operation from N -> N)
--optimised in the sense that it exploits certain algebraic properties such that e.g 3+2 is not generated bc 2+3 was generated
valid :: Op -> Int -> Int -> Bool
valid Add x y = x > y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0 

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = div x y

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "(" ++ show e ++ ")"

--return a list of values contained in the expression
values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

--failure in evaluation is handled by returning a list , where a singletion list denotes success and 
--the returning of the empty list denotes failure
eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <-eval l,
                                  y <-eval r,
                                  valid o x y]

--some combinatorial functions

--powerset
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x: ) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms xs = foldr (concatMap . interleave) [[]] xs

choices :: [a] -> [[a]]
choices = concatMap perms . subs



solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a] ,[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls, rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls,
                                      r <- exprs rs,
                                      e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = 
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

--Making it more efficient 
type Result = (Expr, Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                      lx      <- results ls,
                      ry      <- results rs,
                      res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = 
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e, m) <- results ns', m == n]

