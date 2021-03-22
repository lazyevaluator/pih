--1
data Nat = Zero | Succ Nat
  deriving Show

nat2int :: Nat -> Int
nat2int Zero      = 0
nat2int (Succ n)  = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

--solution of exercise
mult :: Nat -> Nat -> Nat
mult Zero     n = Zero
mult (Succ m) n = add (mult m n) n


data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving Show

--2
--faster bc it makes the comparison once per recursive call instead of 3 times in worst case when using guards.
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) =
  case compare x y of 
    LT -> occurs x l
    EQ -> True
    GT -> occurs x r

--3
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)
  deriving Show
countLeaves :: Tree2 a -> Int
countLeaves (Leaf2 x) = 1
countLeaves (Node2 l r) = countLeaves l + countLeaves r

balanced :: Tree2 a -> Bool
balanced (Leaf2 x)  = True
balanced (Node2 l r) = abs (countLeaves l - countLeaves r) <= 1 
                       && balanced l && balanced r

--4
split :: [a] -> ([a], [a])
split xs = splitAt half xs
  where half = div (length xs) 2

balance :: [a] -> Tree2 a
balance [x] = Leaf2 x
balance xs  = Node2 (balance l) (balance r)
  where (l, r) = split xs

--5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x)     = f x
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

--6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

--7
-- instance Eq a => Eq (Maybe a) where
--   Nothing  == Nothing  = True
--   (Just a) == (Just b) = a == b
--   _        == _        = False

-- instance Eq a => Eq [a] where
--   []     == []     = True
--   (x:xs) == (y:ys) = x == y && (xs == ys)