euclides :: Int -> Int -> Int
euclides x y
    | x == y = x
    | x < y = euclides (y-x) x
    | otherwise = euclides (x-y) y