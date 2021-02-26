tupleProd :: Num a => (a, a) -> a
tupleProd (a,b) = a * b

produtoEscalar :: Num a => [a] -> [a] -> a
produtoEscalar a b = sum ([tupleProd x | x <- (zip a b)])