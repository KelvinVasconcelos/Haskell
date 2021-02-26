coord :: Int -> Int -> [(Int, Int)]
coord n v = [(x', y') | x' <- [0..n], y' <- [0..v]]