--LUIS POZAS PALOMO - SESION 4

--EJERCICIO 1:
data Nat = Cero | Suc Nat deriving (Eq, Ord, Show)

-- He creado los operadores de esta manera para que no afecte
-- a otros ejercicios: (~+) = +, (~*) = *

infix 4 ~+
(~+) :: Nat -> Nat -> Int
a ~+ b = (natToInt a) + (natToInt b)

infix 5 ~*
(~*) :: Nat -> Nat -> Int
a ~* b = (natToInt a) * (natToInt b)

natToInt :: Nat -> Int 
natToInt Cero = 0
natToInt (Suc n) = 1 + natToInt n

show'::Nat -> String
show' a = show (natToInt a)

--EJERCICIO 2:
data Complejo = C Float Float deriving (Eq, Show)

myShow::Complejo -> String
myShow (C a b) = if(b >= 0) then show a ++" + "++ show b ++"i"
                 else show a ++" "++ show b ++"i"

-- He creado los operadores de esta manera para que no haya
-- problemas con el ejercicio anterior (/*) = *, (/-) = -, (/+) = +   

infix 5 /+
(/+) :: Complejo -> Complejo -> String
a /+ b = case (a, b) of 
        (C x y, C xx yy) -> myShow (C (x+xx) (y+yy))

infix 5 /-
(/-) :: Complejo -> Complejo -> String
a /- b = case (a, b) of 
        (C x y, C xx yy) -> myShow (C (x-xx) (y-yy))

infix 6 /*
(/*) :: Complejo -> Complejo -> String
a /* b = case (a, b) of 
        (C x y, C xx yy) -> myShow (C (x*xx-y*yy) (x*yy+y*xx))

--EJERCICIO 3:
class Medible a where
    tamanyo :: a -> Int

instance Medible Bool where
    tamanyo False = 0
    tamanyo True = 1

instance (Medible a) => Medible [a] where
    tamanyo [] = 0
    tamanyo (x:xs) = tamanyo x + tamanyo xs

instance (Medible a, Medible b) => Medible (a, b) where
    tamanyo (x, y) = tamanyo x + tamanyo y

--EJERCICIO 4:
data Direccion = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)
type Punto = (Float, Float)

destino:: Punto -> [Direccion] -> Punto
destino (x, y) [] = (x, y)
destino (x, y) (UP:xs) = destino (x, y+1) xs
destino (x, y) (DOWN:xs) = destino (x, y-1) xs
destino (x, y) (LEFT:xs) = destino (x - 1, y) xs
destino (x, y) (RIGHT:xs) = destino (x + 1, y) xs

trayectoria:: Punto -> [Direccion] -> [Punto]
trayectoria (x, y) [] = []
trayectoria (x, y) (UP:xs) = (x, y+1):trayectoria (x, y+1) xs
trayectoria (x, y) (DOWN:xs) = (x, y-1):trayectoria (x, y-1) xs
trayectoria (x, y) (LEFT:xs) = (x - 1, y):trayectoria (x - 1, y) xs
trayectoria (x, y) (RIGHT:xs) = (x + 1, y):trayectoria (x + 1, y) xs

--EJERCICIO 5
data Arbol_G a = Hoja a | Nodo a (Arbol_G a) (Arbol_G a)

listaHojas:: Arbol_G a -> [a]
listaHojas (Hoja a) = [a]
listaHojas (Nodo a i d) = listaHojas i ++ listaHojas d

listaNodos:: Arbol_G a -> [a]
listaNodos (Hoja a) = [a]
listaNodos (Nodo a i d) = listaNodos i ++ [a] ++ listaNodos d

maxTree:: (Eq a, Ord a) => Arbol_G a -> a
maxTree (Hoja a) = a
maxTree(Nodo a i d)
     | a >= mi && a >= md = a
     | mi >= md && mi >= a = mi
     | otherwise = md
     where mi = maxTree i
           md = maxTree d

f:: (Eq a, Ord a) => Arbol_G a -> a-> Arbol_G a
f (Hoja a) m = Hoja m
f (Nodo a i d) m = Nodo m (f i m) (f d m)

repMax:: (Eq a, Ord a) => Arbol_G a -> Arbol_G a
repMax a = f a (maxTree a)