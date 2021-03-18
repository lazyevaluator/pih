module Ex04 where

{-1-}
halve :: [a] -> ([a], [a])
halve xs | even $ length xs = splitAt half xs
  where half = div (length xs) 2
halve _ = ([],[])

{-2-}

--a
thirdA :: [a] -> a
thirdA as | length as >= 3 = head $ tail $ tail as

--b
thirdB :: [a] -> a
thirdB bs = bs !! 3

--c
thirdC :: [a] -> a
thirdC (_:_:c:cs) = c

{-3-}

--a
safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

--b
safetailB :: [a] -> [a]
safetailB xs  | null xs = []
              | otherwise = tail xs

--c 
safetailC :: [a] -> [a]
safetailC [] = []
safetailC xs = tail xs

{-4-}
True ||| _  = True
_ ||| True  = True
_ ||| _     = False
--rest analogous to && im not going to do this.

{-7-}
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

--mult2 = \x -> (\y -> (\z -> x*y*z))

{-8-}
luhnDouble :: Int -> Int
luhnDouble x  | d > 9 = d - 9
                where d = 2*x
luhnDouble x = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0
