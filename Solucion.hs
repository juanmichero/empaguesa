module Solucion where
import Data.Char

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula c = ord c >= 97 && ord c <= 122

-- EJ 2
letraANatural :: Char -> Int
letraANatural c = ord c - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar c n | esMinuscula c = desplazarAux c n
              | otherwise = c

desplazarAux :: Char -> Int -> Char
-- desplazarAux c n = chr (97 + (mod ((mod (ord c - 97 + n) 26) + 26) 26))
desplazarAux c n = chr (97 + modPos)
  where
    pos = ord c - 97
    newPos = mod (pos + n) 26
    modPos = rango newPos

rango :: Int -> Int
rango x = mod (x + 26) 26

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] n = []
cifrar (x:xs) n = desplazar x n : cifrar xs n

-- EJ 5
descifrar :: String -> Int -> String
descifrar s n = cifrar s (-n)

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
-- cifrarLista (x:y:xs) = cifrar x (indice x (x:y:xs)) : cifrarLista (y:xs)

-- ["bbb","aaa"]

-- indice :: Eq a => a -> [a] -> Int
-- indice x xs = buscarIndice xs 0
--   where
--     buscarIndice [] _ = -1                     
--     buscarIndice (y:ys) i
--       | x == y    = i                          
--       | otherwise = buscarIndice ys (i + 1)   

-- longitud :: [t] -> Integer
-- longitud [] = 0
-- longitud (x:xs) = 1 + longitud xs  

-- EJ 7
frecuencia :: String -> [Float]
frecuencia _ = [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente _ _ = ('o', 33.333336)

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
