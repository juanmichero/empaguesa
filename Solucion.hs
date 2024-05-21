module Solucion where
import Data.Char

-- Completar!
-- Nombre de grupo: {}
-- Integrante1: { DNI1,apellidoYNombre1}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula x | ord x >= 97 && ord x <= 122 = True
              | otherwise = False
-- EJ 2
letraANatural :: Char -> Int
letraANatural c = ord c - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar c n
    |not (esMinuscula c) = c
    |otherwise = chr (97 + mod (letraANatural c + n) 26)

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n = desplazar x n : cifrar xs n

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (x:xs) n = desplazar x (-n) : descifrar xs n

-- EJ 6
cifrarListaAux :: [String] -> Int -> Int -> [String]
cifrarListaAux [] _ _ = []
cifrarListaAux (l:ls) desde hasta
    |desde < hasta = cifrar l desde: cifrarListaAux ls  (desde+1) hasta


cifrarLista :: [String] -> [String]
cifrarLista lista = cifrarListaAux lista  0  (length lista)

-- EJ 7
pertenece ::(Eq t)=>t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs)
    |e==x = True
    |otherwise = pertenece e xs

contarMinusculas :: String -> Float
contarMinusculas [] = 0
contarMinusculas (x:xs)
    | esMinuscula x = 1 + contarMinusculas xs
    |otherwise = contarMinusculas xs

cuantasVecesAparece ::(Eq t) => t -> [t] -> Float
cuantasVecesAparece _ [] = 0
cuantasVecesAparece e (x:xs)
    |e==x = 1 + cuantasVecesAparece e xs
    |otherwise = cuantasVecesAparece e xs

frecuenciaAux :: String -> Int -> Int -> [Float]
frecuenciaAux frase k n
    |k==n+1 = []
    |pertenece letra frase = (cuantasVecesAparece letra frase)/ (contarMinusculas frase)*100 : frecuenciaAux frase (k+1) n
    |otherwise = 0.0 : frecuenciaAux frase (k+1) n
    where
        letra = chr (97+k)

frecuencia :: String -> [Float]
frecuencia frase = frecuenciaAux frase 0 25

-- Ej 8
--requiere que e pertenezca a la lista


maximo ::(Ord t)=> [t] -> t
maximo (x:xs)
    |xs ==[] = x
    |x > head (xs) = maximo (x:tail (xs))
    |otherwise = maximo xs


cifradoMasFrecuente:: String -> Int -> (Char,Float)
cifradoMasFrecuente frase n = (chr (97 + indice (maximo (frecuencia (cifrar frase n))) (frecuencia (cifrar frase n))), maximo (frecuencia (cifrar frase n)))

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado "" "" = True
esDescifrado frase1 frase2 = ciclo frase1 frase2 0

ciclo :: String -> String -> Int -> Bool
ciclo frase1 frase2 n
    |frase1 == frase2 = True
    |n < 27 = ciclo frase1 (cifrar frase2 1) (n+1)
    |otherwise = False

-- EJ 10 
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados lista = descifradosLista lista 0

descifradosLista :: [String] -> Int -> [(String, String)]
descifradosLista lista n
    |n == length lista = []
    |cifradoEnLista (head lista) (tail lista) = tuplaCifrado : descifradosLista rotarLista (n+1)
    |otherwise = descifradosLista rotarLista (n+1)
        where tuplaCifrado = (head lista, cifrado (head lista) (tail lista))
              rotarLista = (tail lista) ++ [(head lista)]

cifrado :: String -> [String] -> String -- Dado un String y una lista, da el cifrado o descifrado de cualquiera de sus pasos, de no haberlo, da un espacio en blanco
cifrado frase [] = ""
cifrado frase lista
    |esDescifrado frase (head lista) = head lista
    |otherwise = cifrado frase (tail lista)

cifradoEnLista :: String -> [String] -> Bool -- Verifica que el cifrado o descifrado de una palabra esté dentro de la lista 
cifradoEnLista _ [] = False
cifradoEnLista frase lista
    |esDescifrado frase (head lista) = True
    |otherwise = cifradoEnLista frase (tail lista)

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave "" _ = ""
expandirClave frase n
    |length frase > n = eliminarExceso frase n
    |otherwise = expandirClave (frase ++ frase) n


eliminarExceso :: String -> Int -> String -- Toma una palabra y recortar los últimos n caracteres
eliminarExceso "" _ = ""
eliminarExceso frase n
    |n == 0 = []
    |otherwise = head frase : eliminarExceso (tail frase) (n-1)

{--rotarFrase :: String -> Int -> String
rotarFrase frase 0 = frase 
rotarFrase frase n = rotarFrase (tail frase ++ [(head frase)]) (n-1)--}

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere ""  ""   = []
cifrarVigenere ""  _    = ""
cifrarVigenere frase "" = frase
cifrarVigenere frase1 frase2 = primeraLetra : cifrarSiguienteLetra
    where primeraLetra = desplazar (head frase1) (letraANatural (head frase2))
          cifrarSiguienteLetra = cifrarVigenere (tail frase1) (tail (expandirClave frase2 (length frase1)))

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere "" ""    = []
descifrarVigenere "" _     = ""
descifrarVigenere frase "" = frase
descifrarVigenere frase1 frase2 = primeraLetra : descifrarSiguienteLetra
    where primeraLetra = desplazar (head frase1) ((letraANatural (head frase2))*(-1))
          descifrarSiguienteLetra = descifrarVigenere (tail frase1) (tail (expandirClave frase2 (length frase1)))

-- EJ 14



--devuelve el valor absoluto de un numero
valorAbsoluto :: Int -> Int
valorAbsoluto x
    |x < 0 = x*(-1)
    |otherwise = x


--dado un elemento y una lista, devuelve su posición en la lista
indice :: (Eq t)=> t ->[t]-> Int
indice e (x:xs)
    |e==x= 0
    |otherwise = 1 + indice e xs

-- dado un indice devuelve el elemento que se encuentra en esa posicion de la lista
seleccionarElementoAux ::(Eq t) => Int -> Int -> [t]-> t 
seleccionarElementoAux k n (l:ls)
    |k == n = l
    |otherwise = seleccionarElementoAux (k+1) n ls

seleccionarElemento :: (Eq t)=> Int -> [t]-> t
seleccionarElemento ind lista = seleccionarElementoAux 0 ind lista 


minimo :: [Int] -> Int
minimo [n] = n
minimo (x:xs)
    | x < head xs = minimo(x: tail xs)
    |otherwise = minimo xs


sumatoria :: [Int]-> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--dado una lista de listas de distancias, devuelve una lista de la sumatoria de las distancias
listaDeSumatorias :: [[Int]] -> [Int]
listaDeSumatorias [] = []
listaDeSumatorias (l:ls) = sumatoria l : listaDeSumatorias ls


--dado una frase y un array de claves, devuelve un array de las distancias de los cifrados vig con cada clave
listaDeDistancias ::String ->[String]-> [[Int]]
listaDeDistancias frase [fraseCifrada]  = [distanciasCifrados frase fraseCifrada]
listaDeDistancias  frase (l:ls) = distanciasCifrados frase l : listaDeDistancias frase ls 


--devuelve un array de int de las distancias de los cifrados vigenere comparado a la frase original
distanciasCifrados :: String -> String -> [Int]
distanciasCifrados (f:fs) (fc:fcs) 
    |fs==[] && fcs == [] = []
    |otherwise = valorAbsoluto(letraANatural fc - letraANatural f) : distanciasCifrados fs fcs

--
listaDeCifrados:: String -> [String] -> [String]
listaDeCifrados frase [c] = [cifrarVigenere frase c]
listaDeCifrados frase (c:cs) = [cifrarVigenere frase c] ++ listaDeCifrados frase cs

peorCifrado :: String -> [String] -> String
peorCifrado frase claves = seleccionarElemento (indice (minimo(listaDeSumatorias(listaDeDistancias frase (listaDeCifrados frase claves)))) (listaDeSumatorias(listaDeDistancias frase (listaDeCifrados frase claves)))) claves

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere _ _ _ = [("hola", "b")]
