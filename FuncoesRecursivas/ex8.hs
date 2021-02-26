--A)--
soma :: [Int] -> Int
soma [x] = x
soma [] = 0
soma (x:xs) = x + soma xs

--B)--
pegar :: Int -> [a] -> [a]
pegar 0 xs = []
pegar y [x] = [x]
pegar y (x:xs) = x : pegar (y-1) xs

--C)--
ultimo :: [Int] -> Int
ultimo [x] = x
ultimo [] = 0
ultimo (x:xs) = ultimo xs