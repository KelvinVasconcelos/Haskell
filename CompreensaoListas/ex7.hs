listaFrst :: [Integer]
listaFrst = [x | x <- [1,2]]

listaScnd :: [Integer]
listaScnd = [y | y <- [3,4]]

listaFnl :: [(Integer, Integer)]
listaFnl = [(x, y) | x <- listaFrst, y <- listaScnd]