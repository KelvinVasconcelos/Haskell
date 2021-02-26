coord :: Int -> Int -> [(Int, Int)]
coord n v = [(x', y') | x' <- [0..n], y' <- [0..v]]

quad :: Int -> [(Int, Int)]
quad n = [(x', y') | (x', y') <- coord n n, x' /= y']
