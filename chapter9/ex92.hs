subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x: ) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms xs = foldr (concatMap . interleave) [[]] xs

choices :: [a] -> [[a]]
choices = concatMap perms . subs

--should be read as xs `isChoice(of)` ys
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice xs []     = null xs
isChoice (x:xs) ys = elem x ys && isChoice xs (removeFirst x ys)


removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ []     = []
removeFirst x (y:ys) = if x == y then ys else y : removeFirst x ys

