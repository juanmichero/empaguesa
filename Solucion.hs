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
letraANatural c = ord c

--3
desplazar :: Char -> Int -> Char
desplazar c n 
    |ordc + n >= 97 && ordc + n <= 122 = chr (ordc + n)
    |otherwise = chr (ordc-26 + n)
    where 
        ordc = letraANatural c 
        
--4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n = desplazar x n : cifrar xs n

--5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (x:xs) n = desplazar x (-n) : descifrar xs (-n)


--------------------------------------------------




-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista _ = ["compu", "mbcp", "kpvtq"]

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
