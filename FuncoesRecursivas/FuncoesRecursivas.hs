--1ª QUESTÃO--

fatorial  ::  Int  ->  Int
fatorial x
    | x <= 0 = 1
    | otherwise = x * fatorial (x-1)

--------------

--2ª QUESTÃO--

somar  ::  Int  ->  Int
somar 0 = 0
somar x = x + somar(x-1)

--------------

--3ª QUESTÃO--

(^^^^) :: (Eq p, Num p) => p -> p -> p
x ^^^^ 0 = 1
x ^^^^ y = x * (x ^^^^ (y-1))

--------------

--4ª QUESTÃO--

euclides :: Int -> Int -> Int
euclides x y
    | x == y = x
    | x < y = euclides (y-x) x
    | otherwise = euclides (x-y) y

--------------

--5ª QUESTÃO--

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

--------------

--6ª QUESTÃO--

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

--------------

--7ª QUESTÃO--

metades :: [a] -> ([a],[a])
metades xs = splitAt lhx xs
        where lhx = length xs `div` 2

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
            where (left,right) = metades xs

--------------

--8ª QUESTÃO--

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

--------------
