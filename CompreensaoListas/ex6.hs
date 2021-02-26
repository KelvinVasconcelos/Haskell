verifica :: Int -> Bool 
verifica n = n == sum [i | i <- [1..n-1], n `mod` i == 0]

perfeitos :: Int -> [Int]
perfeitos n = [a | a <- [1..n], verifica a]