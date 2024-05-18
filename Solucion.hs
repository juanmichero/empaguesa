module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {}
-- Integrante1: { DNI1,apellidoYNombre1}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula 'a' = True
esMinuscula letra 
    |ord letra >= 97 && ord letra <= 122 = True
    |otherwise = False

-- EJ 2
letraANatural :: Char -> Int
letraANatural 'a' = 0
letraANatural letra = ord letra - 97

-- EJ 3
desplazar :: Char -> Int -> Char -- Falta el caso donde hace salto en el abecedario
desplazar letra 0 = letra
desplazar letra n
    |esMinuscula letra && noHaySaltosHaciaA = chr (ord letra + n)
    |esMinuscula letra && noHaySaltosHaciaZ = chr (ord letra + n)
    |esMinuscula letra && haySaltoHaciaA = desplazar 'a' (n - 1 - distanciaDeZ letra)
    |esMinuscula letra && haySaltoHaciaZ = desplazar 'z' (n + 1 + distanciaDeA letra)
    |otherwise = letra
        where noHaySaltosHaciaA = letraANatural letra + n <= 25 && letraANatural letra + n >= 0
              noHaySaltosHaciaZ = letraANatural letra + n <= 25 && letraANatural letra + n >= 0
              haySaltoHaciaA = distanciaDeZ letra < n  
              haySaltoHaciaZ = distanciaDeA letra < -n

distanciaDeZ :: Char -> Int
distanciaDeZ 'z' = 0
distanciaDeZ letra = ord 'z' - ord letra

distanciaDeA :: Char -> Int
distanciaDeA 'a' = 0
distanciaDeA letra = ord letra - ord 'a'

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar frase 0 = frase
cifrar frase n = desplazar (head frase) n : cifrar (tail frase) n

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar frase 0 = frase
descifrar frase n = desplazar (head frase) (-n) : descifrar (tail frase) n

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
--cifrarLista (x:xs) =  

invertirLista :: [String] -> [String] -- /////////// ARREGLAR /////////////////
invertirLista [] = []
invertirLista xs = ultimo xs : sacarUltimo xs

sacarUltimo :: [String] -> [String]
sacarUltimo [x] = []
sacarUltimo (y:ys) = y : sacarUltimo ys

ultimo :: [String] -> [String]
ultimo [x] = [x]
ultimo (y:ys) = ultimo ys
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
