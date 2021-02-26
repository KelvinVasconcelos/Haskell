--A)--
and' :: [Bool] -> Bool
and' (x:xs)
    | xs == [] = x
    | otherwise = x && and' xs

--B)--
concat' :: Eq a => [[a]] -> [a]
concat' (x:xs)
    | (x:xs) == [] = []
    | xs == [] = x
    | otherwise = x ++ concat' xs

--C)--
replicate' :: Int -> Int -> [Int]
replicate' v t
    | t == 0 = []
    | otherwise = v : replicate' v (t-1)

--D)--
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x 
(!!!) (x:xs) e = (!!!) xs (e-1)

--E)--
elem' :: Eq t => t -> [t] -> Bool
elem' v (x:xs)
    | xs == [] = v == x
    | v == x = True
    | otherwise = elem' v xs