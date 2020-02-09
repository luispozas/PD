{- LUIS POZAS PALOMO - HOJA 4 - PROGRAMACION DECLARATIVA -}

{- EJERCICIO 1: Supongamos que definimos el tipo data Pila = P [a] para representar pilas. 
Define funciones (creaPila) para crear una pila vacia, (esPilaVacia) para determinar 
si una pila dada esta vacia o no, (apilar) para apilar un elemento, (cima) para consultar
la cima de una pila no vacia y (desapilar) para eliminar la cima de una pila no vacia. 
Determina el significado de la funcion "r" -}

data Pila a = P [a] deriving (Show)

creaPila :: Pila a
creaPila = P []

esPilaVacia:: Pila a -> Bool
esPilaVacia (P xs) = if length xs == 0 then True else False

apilar:: a -> Pila a -> Pila a
apilar x (P xs) = (P (x:xs))

cima:: Pila a -> a
cima (P (x:xs)) = x

desapilar:: Pila a -> Pila a
desapilar (P (x:xs)) = (P xs)

r :: [a] -> [a]
r xs = ys 
    where (P ys) = foldl (\p x -> apilar x p) creaPila xs

r' :: [a] -> Pila a
r' xs = (P ys) 
    where (P ys) = foldl (\p x -> apilar x p) creaPila xs

{- EJERCICIO 2: Define una funcion primeroQueCumple :: (a -> Bool) ->[a] -> Maybe a, que dada
una propiedad y una lista devuelva el primer elemento de la lista que cumple la propiedad. 
Devuelve Nothing en el caso de que ninguno la cumpla. -}

primeroQueCumple :: (a -> Bool) ->[a] -> Maybe a
primeroQueCumple p [] = Nothing
primeroQueCumple p (x:xs) = if (p x) then Just x else primeroQueCumple p xs

{- EJERCICIO 3: Define un tipo de datos Cj para representar conjuntos de elementos del mismo tipo. 
Define funciones para crear un conjunto vacio, para determinar si un conjunto dado esta vacio o no,
para determinar si un elemento pertenece o no a un conjunto y para devolver la lista con todos los
elementos que pertenecen a un conjunto. Recuerda que en un conjunto no puede haber elementos
repetidos y que el orden de los elementos no importa. -}

data Cj a = Cj [a] deriving (Show)

creaConjVacio:: Cj a
creaConjVacio = Cj []

esConjVacio :: Cj a -> Bool
esConjVacio (Cj []) = True
esConjVacio _ = False

perteneceConj':: (Eq a) => a -> Cj a -> Bool
perteneceConj' x (Cj xs) = foldl (\acc y -> if (y==x) then True else acc) False xs

perteneceConj:: (Eq a) => a -> Cj a -> Bool
perteneceConj x (Cj xs) = elem x xs

incluyeElem :: (Eq a) => [a] -> a -> [a]
incluyeElem xs e 
      | perteneceConj e (Cj xs) = xs
      | otherwise = e:xs

quitaRepes:: (Eq a) => Cj a -> Cj a
quitaRepes (Cj xs) = Cj (foldl incluyeElem [] xs)

listaConj:: (Eq a) => Cj a -> [a]
listaConj conj = xs
    where Cj xs = quitaRepes conj

iguales:: (Eq a) => Cj a -> Cj a -> Bool
iguales (Cj c1) (Cj c2) = (not (any (==False) [(perteneceConj x (Cj c1)) | x <- l2])) && (length l2 == length l1)
                          where l2 = (listaConj (Cj c2))
                                l1 = (listaConj (Cj c1))

instance Eq a => Eq (Cj a) where
  c1 == c2 = iguales c1 c2

{- EJERCICIO 4: Define un tipo para representar matrices de n ́umeros reales.
Escribe una funcion que calcule la transpuesta de una matriz rectangular dada.
Escribe una funcion para calcular la operacion de suma de matrices. -}
type Vector = [Float]
type Matriz = [Vector]

traspuesta:: Matriz -> Matriz
traspuesta m1 = [[(m1!!i)!!j | i <- [0..(length m1)-1]] | j <- [0..(length m1) -1]]

suma:: Matriz -> Matriz -> Matriz
suma m1 m2 = [[((m1!!j)!!i) + ((m2!!j)!!i) | i <- [0..(length m1)-1]] | j <- [0..(length m1)-1]]

{- EJERCICIO 5: Dada la declaracion: data Temp = Kelvin Float | Celsius Float | Fahrenheit Float
para representar temperaturas en diferentes escalas, escribe una funcion para realizar conversiones
de una escala a otra y otra para determinar la escala en la que esta representada una temperatura.
El nuevo tipo tiene que ser instancia de las clases Ord y Eq. Define adecuadamente los metodos == y compare
para la nueva estructura de datos. -}

data Temp = Kelvin Float | Celsius Float | Fahrenheit Float deriving (Show)
ceroAbs = -273.15

toCelsius :: Temp -> Temp
toCelsius (Kelvin x) = Celsius(x + ceroAbs)
toCelsius (Fahrenheit y) = Celsius((y-32)/1.8)
toCelsius c = c

toFahrenheit :: Temp -> Temp
toFahrenheit (Kelvin x) = Fahrenheit(x*2)
toFahrenheit (Celsius c) = Fahrenheit((c*1.8)+32)
toFahrenheit f = f

toKelvin :: Temp -> Temp
toKelvin (Celsius c) = Kelvin(c - ceroAbs)
toKelvin (Fahrenheit f) = Kelvin(f*2)
toKelvin k = k

escala:: Temp -> String
escala(Kelvin k) = "Kelvin"
escala(Fahrenheit f) = "Fahrenheit"
escala(Celsius c) = "Celsius"

igualesC:: Temp -> Temp -> Bool
igualesC x y =
   case (toCelsius x, toCelsius y) of
    (Celsius x', Celsius y') -> x' == y'
    _ -> error "esto no es posible"

instance Eq Temp where
    x == y = igualesC x y
 

{- EJERCICIO 6: Declara adecuadamente un tipo de datos para representar arboles binarios de busqueda con
valores en los nodos pero no en las hojas. Programa en Haskell la ordenacion de una lista
por el algoritmo treeSort, consistente en ir colocando uno a uno los elementos de la lista
en un arbol binario de busqueda inicialmente vacio. 
A continuacion devuelve la lista resultante de recorrer el arbol en inOrden. -}

data MyTree a = H | N a (MyTree a) (MyTree a) deriving (Show, Eq)

createTree :: MyTree a
createTree = H

numElemT:: MyTree a -> Int
numElemT (H) = 0
numElemT (N _ i d) = 1 + (numElemT i) + (numElemT d)

addT:: (Ord a, Eq a) => a -> MyTree a -> MyTree a
addT x (H) = (N x H H)
addT x (N e i d) = if(x < e) then (N e (addT x i) d) else (N e i (addT x d))

addListToTree:: (Ord a, Eq a) => [a] -> MyTree a -> MyTree a
addListToTree (x:xs) tree = addListToTree xs (addT x tree)
addListToTree [] tree = tree

inOrden:: (Ord a, Eq a) => MyTree a -> [a]
inOrden (H) = []
inOrden (N e i d)= inOrden i ++ [e] ++ inOrden d

treeSort :: (Ord a, Eq a) => [a] -> [a]
treeSort xs = inOrden (addListToTree xs createTree)

--------------------------------------------------------------------------------------
--                                 ENTRADA / SALIDA                                 --
--------------------------------------------------------------------------------------
getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int)

adivina:: Int -> IO ()
adivina n = do 
            putStr("Introduce un numero: ")
            a <- getLine
            putStr("\n")
            if(read a::Int) < n then do 
                           putStr("Has introducido un numero menor\n")
                           adivina n
                      else if (read a::Int) > n then do
                                         putStr("Has introducido un numero mayor\n")
                                         adivina n
                                    else do 
                                         putStr("correcto!\n")

cuantasHay:: String -> Int
cuantasHay [] = 1
cuantasHay (x:xs) = if x == ' ' then 1 + cuantasHay xs
                    else cuantasHay xs

wordCount:: IO Int
wordCount = do
            putStr("Introduce una frase: ")
            a <- getLine
            putStr ("\n")
            putStr("El numero de palabras introducidas es: ")
            return (cuantasHay a)
