--LUIS POZAS PALOMO - SESION 5 DE LABORATORIO - FECHA DE ENTREGA 27/12/2019

--EJERCICIO 1:
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

--EJERICCIO 2:
numPalabras:: IO Int
numPalabras = do
            putStr("Introduce una frase: ")
            a <- getLine
            putStr ("\n")
            putStr("El numero de palabras introducidas es: ")
            return (length (words a))

--EJERCICIO 3:
--A) -----------------------------------
palabras:: String -> IO Int
palabras xs = do 
              x <- readFile xs
              return (length (words x))

--B) ------------------------------------
palabras' :: IO String
palabras' = do
            putStr("Introduce el nombre del fichero: ")
            a <- getLine
            putStr("\n")
            b <- readFile a
            return ("El fichero " ++ a ++ " tiene "  ++ show (length (words b)) ++ " palabras.")

--C) -------------------------------------
promedia :: IO ()
promedia = promedia' 0 0

-- s -> almacena la suma actual hasta el momento
-- n -> almacena el numero de elementos hasta el momento
promedia' :: Int -> Int -> IO ()
promedia' s n = do 
                putStr("Introduce un numero: ")
                a <- getLine
                if(read a::Int) >= 0 then do
                                         putStr("La suma es: " ++ show(s+(read a::Int)) ++ ", y la media es: " ++ show ((s+(read a::Int)) `div` (n+1)) ++ "\n")
                                         promedia'(s+read a::Int) (n+1)
                
                                    else putStr("FIN\n")

--D) -------------------------------------
{-
formatea:: String -> String -> Int -> IO ()
formatea fileIn fileOut n = do
                            a <- readFile fileIn
                            lines a
-}

--E) -----------------------------------
--OJO AL ESTUDIAR. Con "read" se le pasa una lista, no un caracter solo.
--Supongo que evaluo la expresion de izquierda a derecha sin prioridad.
operacion :: Int -> Char -> Int -> Int
operacion x '*' y = x * y
operacion x '+' y = x + y
operacion x '/' y = div x y
operacion x '-' y = x - y

evalua :: [Char] -> Int
evalua [x] = read [x]::Int
evalua (x:o:ys) = operacion (evalua ys) o (read [x]::Int)

calculadora :: IO ()
calculadora = do
              putStr("Introduce la operacion: ")
              a <- getLine
              putStr("El resultado es: " ++ show(evalua(concat(words a))) ++ "\n")