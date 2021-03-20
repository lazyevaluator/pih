--1
--show how [f x | x <- xs, p x] can be re-expressed using map and filter
fun :: (a -> a) -> (a -> Bool) -> [a] -> [a]
fun f p = map f . filter p

--2
all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p bs = and $ map p bs

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p bs = or $ map p bs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []     = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []     = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else x:xs

--3 redefine map and filter using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

--4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

--5
curry':: ((a,b) -> c) -> (a -> b -> c)
curry' f = \a -> (\b -> f (a,b))

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(a,b) -> f a b

--9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

--10
luhn :: [Int] -> Bool
