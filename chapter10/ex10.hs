import Prelude hiding (putStr)
import Data.Char
import System.IO (hSetEcho, stdin)
putStr :: String -> IO ()
putStr s = sequence_ [putChar c | c <- s]

--2
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard' :: Board -> IO ()
putBoard' xs = do putBoard'' xs 1

putBoard'' :: Board -> Int -> IO ()
putBoard'' [] _     = putChar '\n'
putBoard'' (x:xs) n = do putRow n x
                         putBoard'' xs (n+1)

--3
putBoard :: Board -> IO ()
putBoard xs = sequence_ [putRow n x | (n,x) <- zip [1..(length xs)] xs] 

--4
adder1 :: IO ()
adder1 = do putStrLn "Enter the amount of numbers you want to add: "
            x <- getLine
            adder' (read x :: Int) 0

adder' :: Int -> Int -> IO ()
adder' 0 m = do putStr "The total is: "
                print m
adder' n m = do x <- getLine
                adder' (n-1) (m + (read x :: Int))

--5
readInt :: IO Int
readInt = do
  x <-getLine
  return (read x :: Int)

readInts :: Int -> IO [Int]
readInts n = sequence [readInt | _ <- [1..n]]

ader :: IO ()
ader = do putStr "How many numbers ? "
          n <- readInt
          ns <- readInts n
          putStr "The total is "
          print (sum ns)
          return ()

--6
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = do x <- getCh
              case x of 
                '\n' ->   do putChar x
                             return []
                -- '\DEL' -> do putChar '\b'
                --              readLine
                c ->      do putChar c
                             cs <- readLine
                             return (c:cs)