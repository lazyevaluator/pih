import Data.Monoid
import Data.Foldable

{-
--1
instance (Monoid a, Monoid b) => Monoid (a, b) where
  --mempty :: (a, b)
  mempty = (mempty, mempty)

  --mappend :: (a, b) -> (a, b) -> (a, b)
  (x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)


--2 
instance Monoid b => Monoid (a -> b) where
  --mempty :: (a -> b)
  mempty = const mempty
  --mappend :: (a -> b) -> (a -> b) -> (a -> b)
  f `mappend` g = \x -> f x `mappend` g x

--3
instance Foldable Maybe where
    -- fold :: Monoid a => Maybe a -> a
    fold Nothing = mempty
    fold (Just a) = a

    -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
    foldMap _ Nothing = mempty
    foldMap f (Just a) = f a

    -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
    foldr _ _ Nothing = mempty
    foldr f v (Just a) = f a v

    -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
    foldl _ _ Nothing = mempty
    foldl f v (Just b) = f v b

instance Traversable Maybe where
    -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse g Nothing = pure Nothing
    traverse g (Just a) = Just <$> g a

-}
--4
instance Semigroup Integer where
  x <> y = x `mappend` y

instance Monoid Integer where
  mempty = 0
  x `mappend` y = x + y

tree :: Tree Integer
tree = Node (Node Leaf 1 (Node Leaf 2 Leaf)) 3 (Node (Node Leaf 5 Leaf) 6 Leaf)
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
  --fmap :: (a -> b) -> Tree -> Tree b
  fmap _ Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree where
  --fold :: Monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l <> x <> fold r

  --foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f Leaf = mempty
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

  --foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ v Leaf = v
  foldr f v (Node l x r) = foldr f (foldr f (f x v) r) l
  

  --foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ v Leaf = v
  foldl f v (Node l x r) = foldl f (foldl f (f v x) l) r

instance Traversable Tree where
  --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse g (Node l x r) = Node <$> traverse g l <*> g x <*> traverse g r

--5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> if p x then [x] else mempty)