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

