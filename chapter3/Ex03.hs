module Ex03 where
{-exercise 2-}
apply :: (a -> b) -> a -> b 
apply f = f

{-exercise 3-}
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y, x)

pair :: a -> b -> (a, b)
pair a b = (a, b)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{-exercise 5-}
--functions cannot be of type Eq because two functions f and g are equivalent if for every input they produce the same output, that is
--f a == g a for all a. There being infinitely many integers makes checking this property impossible.
