-- LUIS POZAS PALOMO, SESION 1

-- APARTADO 1: Hacer multiplicaciones y divisiones...
-- APARTADO 2:
--last [1..10^5] 
--last [1..10^7] 
--last [1..10^20] 
--head [1..10^20] 
--last [10^20..1] 
--head (tail [1..10^20]) 
--length [1..10^20] 
--last (take (10^7) [1..10^20]) 
--head (take (10^7) ([1..100] ++ [1..10^20])) 
--last (take 100 ([1..10^20] ++ [1..100])) 
--last (drop 100 ([1..10^20] ++ [1..100])) 
--head (drop (10^7) ([1..10^20] ++ [1..100])) 
--[1..10^7]==[1..10^7] 
--[1..10^20]==[1..10^20] 
--[1..10^20]==[1..10^20+1]
--[1..10^20]==[2..10^20] 
--head (reverse [1..10^7]) 
--last (reverse [1..10^7])
--reverse [1..10^20] == reverse [1..10^20+1]

-- APARTADO 3:
media::[Int] -> Int
media xs = fromIntegral ((sum xs) `div` (length xs))

-- APARTADO 4:
digitos::Int ->Int
digitos 0 = 0
digitos x = 1 + digitos (x`div`10)
--
sumDig:: Int -> Int
sumDig x | x < 10 = x
         | otherwise = (mod x 10) + sumDig (x`div`10)

reduccion:: Int -> Int
reduccion x |x < 10 && x >= 0 = x
            |otherwise = reduccion (sumDig (abs x))
--
perm:: Int -> Int
perm 0 = 1
perm 1 = 1
perm x = x * perm (x-1)
--
var::Int ->Int ->Int
var n m | n < m = 0
        |otherwise = (perm m) `div` (perm(n-m))
--
comb::Int->Int->Int
comb n m | n < m = 0
         |otherwise = (perm n) `div` ((perm m)*(perm (n-m)))

--APARTADO 5:
conj::Bool->Bool->Bool
conj False x = False
conj _ False = False
conj _ _ = True
