module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf

--1
esMinuscula :: Char -> Bool
esMinuscula c
    |ord c >= 97 && ord c <= 122 = True
    |otherwise = False
    
--2
letraANatural :: Char -> Int
letraANatural c = ord c - 97

--3
desplazar :: Char -> Int -> Char
desplazar c n
    |not (esMinuscula c) = c 
    |otherwise = chr(97 + mod (valorLetra + n) 26)
    where
        valorLetra = letraANatural c
        
--4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n = desplazar x n : cifrar xs n

--5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (x:xs) n = desplazar x (-n) : descifrar xs n




-- EJ 6

cifrarLista :: [String] -> [String]
cifrarLista lista = cifrarListaAux lista (length lista) 0
cifrarListaAux :: [String] -> Int -> Int -> [String]
cifrarListaAux [] _ _  = []
cifrarListaAux (l:ls) longitud it
    |it < longitud = cifrar l it : cifrarListaAux ls longitud (it+1)
-- EJ 7
pertenece ::(Eq t)=>t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs)
    |e==x = True
    |otherwise = pertenece e xs

contarMinusculas :: String -> Int
contarMinusculas [] = 0
contarMinusculas (x:xs)
    | esMinuscula x = 1 + contarMinusculas xs
    |otherwise = contarMinusculas xs

cuantasVecesAparece ::(Eq t) => t -> [t] -> Int
cuantasVecesAparece _ [] = 0
cuantasVecesAparece e (x:xs)
    |e==x = 1 + cuantasVecesAparece e xs
    |otherwise = cuantasVecesAparece e xs

frecuenciaAux :: String -> Int -> Int -> [Float]
frecuenciaAux frase k n 
    |k==n+1 = []
    |pertenece letra frase = (fromIntegral(cuantasVecesAparece letra frase)*100/ fromIntegral(contarMinusculas frase) ): frecuenciaAux frase (k+1) n
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
maximo [n] = n
maximo (x:xs)
    |x > head(xs) = maximo(x:tail(xs))
    |otherwise = maximo xs 


cifradoMasFrecuente:: String -> Int -> (Char,Float)
cifradoMasFrecuente frase n = (chr(97 + indice(maximo(frecuencia(cifrar frase n))) (frecuencia(cifrar frase n))), maximo(frecuencia(cifrar frase n)))
  


-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado _ _ = False

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados _ = [("compu", "frpsx"), ("frpsx", "compu")]

-- EJ 11


expandirClaveAux :: String -> String -> Int -> Int -> String
expandirClaveAux frase (x:xs) k n
    |k==n = ""
    |xs == [] = x:expandirClaveAux frase frase (k+1) n
    |otherwise = x:expandirClaveAux frase xs (k+1) n


expandirClave :: String -> Int -> String
expandirClave frase n = expandirClaveAux frase frase 0 n  


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

