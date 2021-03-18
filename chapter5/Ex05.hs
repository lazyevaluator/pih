module Ex05 where

import Test.QuickCheck
import Data.Char

{-1-}
res = sum [x^2 | x <- [1..100]]

{-2-}
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]

{-3-}
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

{-4-}
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

{-5-}
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- xs, y <- xs, z <- xs, x^2 + y^2 == z^2]
  where xs = [1..n]

{-6-}
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> Bool
perfect n = sum xs == n
  where xs = init $ factors n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

{-7-}
--(if you are using stack) to test this witch QuickCheck just type "stack ghci --package QuickCheck" instead of "stack ghci" Then run quickCheck 
--prop_eq in the console 
prop_eq_7 xs ys = 
  [(x,y) | x <- xs, y <- ys] == concat [[(x,y) | y <- ys] | x <- xs]

{-8-}
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [i | (x',i) <- zip xs [0..], x == x']

--task was to redefine positions' with find
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x $ zip xs [0..]

prop_eq_8 x xs =
  positions x xs == positions' x xs

{-9-}
scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

{-10-}


-- Encoding and decoding

let2int :: Char -> Int
let2int c | isUpper c = ord c - ord 'A'
          | isLower c = ord c - ord 'a'
          | otherwise = ord c

int2let :: Int -> Bool -> Char
int2let n False = chr (ord 'a' + n)
int2let n True  = chr (ord 'A' + n) 

--int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isUpper c = int2let ((let2int c + n) `mod` 26) True
          | isLower c = int2let ((let2int c + n) `mod` 26) False
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Frequency analysis

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = length $ filter isAlpha xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
           where
              factor = head (positions (minimum chitab) chitab)
              chitab = [chisqr (rotate n table') table | n <- [0..25]]
              table' = freqs xs
