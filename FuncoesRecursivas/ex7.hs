merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys
    
metades :: [a] -> ([a],[a])
metades xs = splitAt lhx xs
        where lhx = length xs `div` 2

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
            where (left,right) = metades xs