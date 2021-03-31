data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval1 :: Expr -> Maybe Int
eval1 (Val n)   = Just n
eval1 (Div x y) = case eval1 x of 
                      Nothing -> Nothing
                      Just n -> case eval1 y of 
                                    Nothing -> Nothing
                                    Just m -> safediv n m

eval2 :: Expr -> Maybe Int
eval2 (Val n)   = Just n
eval2 (Div x y) = eval x >>= \n ->
                  eval y >>= \m -> 
                  safediv n m

eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m

  
--Our State type is just an integer.
type State = Int

newtype ST a = S (State -> (a, State))

--remove dummy construcor
app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  --pure :: a -> ST a
  pure x = S (\s -> (x,s))

  {- 
  <*> applies a state transformer that returns a function (ST (a->b)) to a state transformer 
  that returns an argument (ST a) to give a state transformer that returns the result of applying the function to the argument
  
  so this kind of applies the function inside the state.
   -}

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
    let (f, s')  = app stf s
        (x, s'') = app stx s' in (f x, s''))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

{-
  from Hutton's book :
  In this manner, the bind (>>=) operator for the state monad integrates the sequencing of state transformers with the
  the processing of their result values.
-}


--Relabelling trees example

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show


tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

fresh :: ST Int
fresh = S (\n -> (n,n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r 