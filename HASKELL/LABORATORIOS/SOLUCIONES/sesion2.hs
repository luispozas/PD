-- LUIS POZAS PALOMO, SESION 2

-- EJERCICIO 1
--a
cuadrados:: (Ord a, Num a) => a -> [a]
cuadrados n
    | n < 0 = []
    | otherwise = (cuadrados (n - 1)) ++ [n^2]

--b
tuplaCuadrados:: (Ord a, Num a) => a -> [(a, a)]
tuplaCuadrados n
    | n < 0 = []
    | otherwise = [(n^2, n)] ++ (tuplaCuadrados (n - 1))

--c
sumaSeno :: Double
sumaSeno = f1 100
         where f1 i
                | i < 0 = 0
                | otherwise = i * abs(sin(i)) + f1 (i-1) 

--d
potencias :: (Num a, Num b, Ord a) => a -> b
potencias n = f1 n 0
         where f1 n i
                | (3^i) < n = if (mod (3 ^i) 100) == 67 then 1 + (f1 n (i+1)) else f1 n (i+1)
                | otherwise = 0

--e
sumaMultiplos :: Integer
sumaMultiplos = f1 1000 0
         where f1 n i
                | i < n = if (mod i 5) == 0 || (mod i 3) == 0 then i + (f1 n (i+1)) else f1 n (i+1)
                | otherwise = 0

--EJERCICIO 2
--a
filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a],[a])
filter2 xs p q = ((filter p xs), (filter q xs))

--b
filters :: [a] -> [(a -> Bool)] -> [[a]]
filters xs [] = []
filters xs (p:ps) = (filter p xs):(filters xs ps)

--c
mapx :: a -> [(a -> b)] ->[b]
mapx x [] = []
mapx x (p:ps) = (p x):(mapx x ps)

--d
iguales :: (Enum a, Eq b) => (a -> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = all (p) [n..m]
          where p x = (f x) == (g x)

--e
cuantos :: (a -> Bool) -> [a] -> Int
cuantos p xs = length(filter p xs)

--f
menorA :: (Enum a) => a -> a -> (a -> Bool) -> [a]
menorA n m p = take 1 (filter p [n..m])

--g
mayor :: (Enum a) => a -> (a -> Bool) -> a
mayor n p = head (filter p [n,(pred(n))..])

--h
ex :: (Enum a) => a -> a -> (a -> Bool) -> Bool
ex n m p = any p [n..m]

--3

--last
last' :: [a] -> a
last' (x:xs) = foldl (\_ x -> x) x xs

--reverse
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

--all
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\acc x -> acc && (p x)) True

--minimum
minimum' :: Ord a => [a] -> a
minimum' (x:xs) = foldl (\acc x -> min acc x) x xs 

--map
map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\acc x -> acc ++ [f x]) []

--filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if(p x) then x:acc else acc) []

--takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x acc -> if(p(x)) then x:acc else []) []

--(+++)
(+++) :: [a] -> [a] -> [a]
(+++) ps xs = foldr (\p ps -> p:ps) xs ps

--Ejercicio 4
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f (x:xs) = foldr f x xs

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs) = foldl f x xs

--Ejercicio 5
cuadrados'' :: (Enum a, Num a) => a -> [a]
cuadrados'' n = map (^2) [0..n]

tuplaCuadrados'' :: (Enum a, Num a) => a -> [(a, a)]
tuplaCuadrados'' n = zip [0..n] (cuadrados'' n)

sumaSeno'' :: Double
sumaSeno'' = sum  $ map((\i -> i * abs(sin(i)))) [1..100]