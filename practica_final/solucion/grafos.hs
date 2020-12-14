{- LUIS POZAS PALOMO - PROGRAMACION DECLARATIVA - PRACTICA FINAL (GRAFOS) -}

data Vertice = A|B|C|D|E|F deriving (Show, Eq, Read)
data Grafo = G [Vertice] [(Vertice, Vertice)] deriving (Read, Show)


g1 = G [B, D, E, C] [(D,E),(E,B),(C,B),(E,C)]
g2 = G [D, F, E] [(D,F),(E,D),(D,E),(F,E)]
g3 = G [A, C, D] [(A,C),(C,D),(A,D)]
g4 = G [A, B, C, D, E]  [(A,D),(A,B),(A,C),(B,C),(C,D),(D,B),(D,E),(E,A)]
g5 = G [A, F, B] [(A,F),(B,A),(A,B),(F,B)]
g6 = G [F, B, D] [(F,B),(B,D),(F,D)]


g7 = G [A, B, C, D] [(B,A), (C,A), (C,D),(D,C)]
g8 = G [A, B, C, D] [(B,C), (C,B), (C,D),(D,B)]



{------------------------ Primera parte: -----------------------------------------------------}
{- 
   En esta funcion compruebo que no existen vertices repetidos mirando
   en una lista auxiliar que tomo como conjunto en el cual voy metiendo
   todo vertice nuevo que no ha aparecido antes. De manera similar hago 
   con las aristas de tal forma que creo este ultimo conjunto añadiendo 
   tanto el vertice origen como el final de cada arista y comprobando si 
   existe en el conjunto de vertices. Si todo ha ido correcto entonces
   es un grafo (True), en caso contrario no (False).
-}

es_grafo:: Grafo -> Bool
es_grafo (G [] _) = False
es_grafo (G v a) = no_vertices_rep v [] && esta_arista v (concat [[x, y] | (x, y) <- a])

no_vertices_rep::[Vertice] -> [Vertice] -> Bool
no_vertices_rep [] l = True
no_vertices_rep (x:xs) l = if elem x l then False else no_vertices_rep xs (x:l)

esta_arista::[Vertice] -> [Vertice] -> Bool
esta_arista v [] = True
esta_arista v (l:ls) = elem l v && (esta_arista v ls)

{- 
   En esta funcion inicializo a "0" una matriz cuadrada del tamaño del numero
   de vertices y por cada arista del grafo actualizo la matriz. Utilizo funcion
   de "getIdx" para poder acceder al elemento correcto de la matriz a traves de
   su vertice.
-}

mat_ady:: Grafo -> [[Int]]
mat_ady (G v a) = mat_ady' (G v a) (inicializar_matriz (length v))

mat_ady':: Grafo -> [[Int]] -> [[Int]]
mat_ady' (G v []) m = m
mat_ady' (G v ((o, d):as)) m = mat_ady' (G v as) (cambiar_elem (getIdx v o) m (cambiar_elem (getIdx v d) (m !! (getIdx v o)) 1))

inicializar_matriz:: Int -> [[Int]]
inicializar_matriz n = [take n [0, 0..] | i <- [0..n-1]]

cambiar_elem:: Int -> [a] -> a -> [a]
cambiar_elem i xs e = let (ys, zs) = splitAt i xs in ys ++ [e] ++ tail zs

getIdx::[Vertice] -> Vertice -> Int
getIdx xs v = buscar v (zip [0..length xs] xs)

getVertice::[Vertice] -> Int -> Vertice
getVertice xs i = buscar i (zip xs [0..length xs])

buscar:: (Eq a) => a -> [(b, a)] -> b
buscar v (x:xs)
    | v == snd(x) = fst(x)
    | otherwise = buscar v xs

{- 
   En estas dos funciones llamo a una tercera para simplificar codigo
   de tal manera que le digo que grado quiero que me calcule ("p" = positivo)
   y ("n" = negativo) y voy creando una lista de enteros (inicializada a "0")
   en la que voy sumando uno por cada arista encontrada. Los grados positivos
   se calculan a traves del origen "o" de la arista y los grados negativos a traves
   de la de destino "d".
-}

grados_pos:: Grafo -> [(Vertice, Int)]
grados_pos (G v a) = grados (G v a) (take (length v) [0,0..]) 'p'

grados_neg:: Grafo -> [(Vertice, Int)]
grados_neg (G v a) = grados (G v a) (take (length v) [0,0..]) 'n'

grados:: Grafo -> [Int] -> Char -> [(Vertice, Int)]
grados (G v []) l c = zip v l
grados (G v ((o, d):as)) l t
    | t == 'p' = grados (G v as) (cambiar_elem (getIdx v o) l (l!!(getIdx v o)+1)) t
    | otherwise = grados (G v as) (cambiar_elem (getIdx v d) l (l!!(getIdx v d)+1)) t

{- 
   En esta funcion llamo a camino_lng que realiza todas las posibles permutaciones de longitud "n" de los vertices
   y para cada una comprueba si es un camino del grafo, quedandose asi con los caminos buenos.
-}

eliminarRepetidos::  (Eq a) => [a] -> [a]
eliminarRepetidos [] = []
eliminarRepetidos (vs:vss) = vs : eliminarRepetidos (filter (/= vs) vss)

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

permutaciones :: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = 
    concat [intercala x ys | ys <- permutaciones xs]

existeArista:: [(Vertice, Vertice)] -> (Vertice, Vertice) -> Bool
existeArista as a = length [ar | ar <-as, ar == a] > 0

--Dado una lista de vertices devuelve una lista de aristas que los unen en ese orden.
creaListaAristas:: [Vertice] -> [(Vertice, Vertice)] 
creaListaAristas vs = zip [a | a <- take ((length vs) -1) vs] [b | b <- drop 1 vs]

esCamino :: Grafo -> [Vertice] -> Bool
esCamino (G vs as) vss = and (map (existeArista as) (creaListaAristas vss))

camino_lng:: Grafo -> Vertice -> Int -> [[Vertice]]
camino_lng (G vs as) v n = eliminarRepetidos [i | i <- [take n xs | xs <- permutaciones vs], head i == v, esCamino (G vs as) i]


{- 
   En esta funcion mira todos los posibles caminos que hay para cada vertice y cada longitud n,
   a continuacion comprueba que la longitud de esa lista de vertices es igual a la lista de vertices del grafo,
   de esta manera nos indica que todos los vertices son accesibles desde ese vertice.
-}

conexo::Grafo -> Bool
conexo g = or (conexoCaminosBuenos g)

-- Devuelve True si existe un camino conexo dada la estructura pasada por parametro.
-- Imprescindible para Entrada-Salida.
esCorrecto:: (Bool, (Vertice, Int)) -> Bool 
esCorrecto a = fst a == True

-- Filtra los caminos buenos llamando a "esCorrecto" quedandose asi con los buenos. Imprescindible para Entrada-Salida.
-- Devuelve para cada camino el vertice inicial y la longitud usada para cada camino formado que uno todos los vertices.
conexoDesdeDonde:: Grafo -> [(Bool, (Vertice, Int))] 
conexoDesdeDonde (G vs as) = filter (esCorrecto) (zip (conexoCaminosBuenos(G vs as)) [(v, i) | v <- vs, i <- [1..(length as)]])

conexoCaminosBuenos::Grafo -> [Bool]
conexoCaminosBuenos (G vs as) = [length (eliminarRepetidos(concat(camino_lng (G vs as) x y))) == length vs | x <- vs, y <- [1..(length as)]]



{-
   Añadir Eq para grafos de tal forma que dos grafos son iguales si son isomorfos.
   Dos grafos son isomorfos si:
       1) Tienen los mismos numeros de vertices.
       2) Tienen los mismos numeros de aristas.
       3) Tienen los mismos grados de entrada y de salida.
-}

mismosGrados:: Grafo -> Grafo -> Bool
mismosGrados g1 g2 = (or[grados == p | p <- permutaciones(zip (map snd (grados_pos g2)) (map snd (grados_neg g2)))])
                   where grados = zip (map snd (grados_pos g1)) (map snd (grados_neg g1))

isomorfos:: Grafo -> Grafo -> Bool
isomorfos (G v1 a1) (G v2 a2) = (length v1 == length v2) && 
                                (length a1 == length a2) && 
                                mismosGrados (G v1 a1) (G v2 a2)

instance Eq (Grafo) where
  g1 == g2 = isomorfos g1 g2

{------------------------ Segunda parte: -------------------------------------------------------------}
{- 
   Funcion que lee primero los vertices y a continuacion las aristas y 
   devuelve el grafo si los datos son correctos, en caso contrario le pide
   que los vuelva a introducir.
-}

leeGrafo:: IO (Grafo)
leeGrafo = do 
           putStr("Introduce la lista de vertices: ")
           v <- getLine
           putStr("Introduce la lista de aristas: ")
           a <- getLine
           if es_grafo (G (read v::[Vertice]) (read a::[(Vertice, Vertice)])) then do
                    return (G (read v::[Vertice]) (read a::[(Vertice, Vertice)]))
                    else do
                         putStr("Grafo incorrecto, vuelva a introducir los datos.\n")
                         leeGrafo

{- 
   Funcion en la que muestra la matriz de adyacencia ademas de añadir los vertices
   que corresponden a cada fila y columna para poder visualizar las aristas
   de manera mas clara, llamo a la funcion "getVertice" para poder obtener el 
   nombre del vertice a traves de los indices de la matriz.
-}

muestra_matriz:: IO()
muestra_matriz = do
                 g <- leeGrafo
                 ver_matriz g (mat_ady g) 0


ver_matriz:: Grafo -> [[Int]] -> Int -> IO()
ver_matriz (G v a) [] i = do
                          putStr("\n      " ++ show v ++ "\n\n")
ver_matriz (G v a) (m:ms) i= do
                             putStr("  " ++ show(getVertice v i) ++ " > " ++ show m ++ "\n")
                             ver_matriz (G v a) ms (i+1)

{-
   Funcion que busca a traves de "conexoDesdeDonde" un vertice y una longitud para buscar
   el camino que se forma y poder mostrarlo por la pantalla.
-}

muestra_caminos:: IO()
muestra_caminos = do
                  g <- leeGrafo
                  if(conexo g) then do
                       ver_caminos (listarCamino g)
                       else do 
                          putStr("El grafo NO es conexo.\n")

listarCamino::Grafo -> [[Vertice]]
listarCamino g = camino_lng g v n
                 where t = conexoDesdeDonde g
                       v = fst(snd (head t))
                       n = snd(snd (head t))

mostrar_un_camino:: [Vertice] -> Int -> IO()
mostrar_un_camino [] _ = do  putStr("  ")
mostrar_un_camino (x:xs) ini = do
                           if(ini == 0) then do
                                             putStr(show x)
                                             mostrar_un_camino xs (ini + 1)
                                        else do 
                                             putStr("-" ++ show x)
                                             mostrar_un_camino xs ini
                           
ver_caminos:: [[Vertice]] -> IO()
ver_caminos [] = do putStr("\n" )
ver_caminos (x:xs) = do
                       mostrar_un_camino x 0
                       ver_caminos xs