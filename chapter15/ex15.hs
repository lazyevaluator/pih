--4
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
