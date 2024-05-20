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
indice :: (Eq t)=> t ->[t]-> Int
indice e (x:xs)
    |e==x= 0
    |otherwise = 1 + indice e xs

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
esDescifrado _ _ = False

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados _ = [("compu", "frpsx"), ("frpsx", "compu")]

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave _ _ = "compucom"

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere _ _ = "kdueciirqdv"

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere _ _ = "computacion"

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ _ = "asdef"

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere _ _ _ = [("hola", "b")]
