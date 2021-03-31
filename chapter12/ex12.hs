

--1
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
  --fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf         = Leaf
  fmap g (Node l x r) = let h = fmap g in Node (h l) (g x) (h r)


--2
instance Functor ((->) r) where
  --fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap = (.)

--3
instance Applicative ((->) r) where
  --pure :: a -> (r -> a)
  pure = const

  -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
  g <*> h = \x -> g x (h x)


--4
newtype ZipList a = Z [a]
  deriving Show

instance Functor ZipList where
  --fmap :: (a -> b) -> Ziplist a -> Ziplist b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  --pure :: a -> Ziplist a
  pure x = Z (repeat x)
  -- (<*>) :: ZipList (a -> b) -> Ziplist a -> Ziplist b
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

--5
{-
pure :: a -> f a
<*> :: f (a -> b) -> f a -> f b

1: pure id <*> x = x :: 
- id :: (a -> a)
- pure id :: f (a -> a)
- x :: f a :: pure id <*> x

2: pure (g x) = pure g <*> pure x
- x :: a
- g :: a -> b
- g x :: b
- pure (g x) :: f b

- pure g :: f (a -> b)
- pure x :: f a
- pure g <*> pure x :: f b

3: x <*> pure y = pure (\g -> g y) <*> x
- x :: f (a -> b)
- y :: a
- pure y :: f a
- x <*> pure y :: f b

- (\g -> g y) :: (a -> b) -> b
- pure (\g -> g y) :: f ((a -> b) -> b)
- pure (\g g y) <*> x :: f b

4: x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
- y :: f (a -> b)
- z :: f a
- y <*> z :: f b
- x :: f (b -> c)
- x <*> (y <*> z) :: f c

- (.) :: (b -> c) -> (a -> b) -> (a -> c)
- pure (.) :: f (((b -> c) -> (a -> b)) -> (a -> c))
- pure (.) <*> x :: f ((a -> b) -> (a -> c))
- pure (.) <*> x <*> y :: f (a -> c)
- (pure (.) <*> x <*> y) <*> z :: f c
-}

--6

instance Monad ((->) r) where
  --return :: a -> (r -> a)
  return = pure

  -- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
  f >>= g = \x -> g (f x) x


--7

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  --fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var a) = Var (g a)
  fmap _ (Val a) = Val  a
  fmap g (Add l r) = Add (fmap g l) (fmap g r)

instance Applicative Expr where
  --pure :: a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _ <*> Val x = Val x
  Val x <*> _ = Val x
  Var f <*> Var x = Var (f x)
  Var f <*> Add x y = Add (fmap f x) (fmap f y)
  Add f g <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
  -- return :: a -> Expr a
  return = pure

  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Val x >>= _   = Val x
  Var x >>= f   = f x
  Add x y >>= f = Add (x >>= f) (y >>= f)