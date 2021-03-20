--2
sumdown :: Int -> Int
sumdown n  
  | n <= 0 = 0
  | n >= 0 = n + sumdown (n-1)

--3 
(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * (m ^^^ (n-1))

--4
euclid :: Int -> Int -> Int
euclid m n 
  | m == n    = n
  | m <= n    = euclid m (n-m)
  | otherwise = euclid n m

--6
--these implementations should be fine, but i'm too bothered by VsCode saying that i should use foldr, so i just commented them out

-- myAnd :: [Bool] -> Bool
-- myAnd [] = True
-- myAnd (x:xs) = x && myAnd xs

-- myConcat :: [[a]] -> [a]
-- myConcat [] = []
-- myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n-1) x

(!!!) :: [a] -> Int -> a
(a:as) !!! 0 = a
(a:as) !!! n = as !!! (n-1)

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (a:as) = x == a || myElem x as

--7
merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys 

--8
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right) 
  where left      = fst splitList
        right     = snd splitList
        splitList = splitAt half xs
        half      = div (length xs) 2

--9
-- mySum :: Num a => [a] -> a
-- mySum []      = 0
-- mySum (x:xs)  = x + mySum xs

myTake :: Int -> [a] -> [a]
myTake 0 _      = []
myTake _ []     = []
myTake n (x:xs) = x : myTake (n-1) xs

myLast :: [a] -> a
myLast [x]    = x
myLast (x:xs) = myLast xs 