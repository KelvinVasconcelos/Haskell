--1ª QUESTÃO--
soma :: Integer
soma = sum [x^2 | x <- [1 .. 100]]

--2ª QUESTÃO--
coord :: Int -> Int -> [(Int, Int)]
coord n v = [(x', y') | x' <- [0..n], y' <- [0..v]]

--3ª QUESTÃO--
quad :: Int -> [(Int, Int)]
quad n = [(x', y') | (x', y') <- coord n n, x' /= y']

--4ª QUESTÃO--
rep :: Int -> a -> [a]
rep n v = [v | x <- [1..n]]

--5ª QUESTÃO--
pitag :: Int -> [(Int, Int, Int)]
pitag n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], (a^2 + b^2) == c^2]

--6ª QUESTÃO--
verifica :: Int -> Bool 
verifica n = n == sum [i | i <- [1..n-1], n `mod` i == 0]

perfeitos :: Int -> [Int]
perfeitos n = [a | a <- [1..n], verifica a]

--7ª QUESTÃO--
listaFrst :: [Integer]
listaFrst = [x | x <- [1,2]]

listaScnd :: [Integer]
listaScnd = [y | y <- [3,4]]

listaFnl :: [(Integer, Integer)]
listaFnl = [(x, y) | x <- listaFrst, y <- listaScnd]

--8ª QUESTÃO--
buscar  ::  Eq  a  =>  a  -> [( a , b )] -> [ b ]
buscar k xs = [v | (k', v) <- xs, k == k']

posicoes  ::  Eq  a  =>  a  -> [ a ] -> [ Int ]
posicoes x xs = buscar x [(z, i) | (z, i) <-  zip xs [ 0  .. ]]

--9ª QUESTÃO--
tupleProd :: Num a => (a, a) -> a
tupleProd (a,b) = a * b

produtoEscalar :: Num a => [a] -> [a] -> a
produtoEscalar a b = sum ([tupleProd x | x <- (zip a b)])
