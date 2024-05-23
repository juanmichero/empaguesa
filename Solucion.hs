module Solucion where
import Data.Char

-- Completar!
-- Nombre de grupo: { Empaguesa }
-- Integrante1: { 43876390,MicheroJuanPedro }
-- Integrante2: { 96229011,OntónEncaladaFavioAndré }
-- Integrante3: { 44935935,PirrelloAgustinNicolas }
-- Integrante4: { 96055051,AularJuan }

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula letra | ord letra >= 97 && ord letra <= 122 = True
                  | otherwise = False

-- EJ 2
letraANatural :: Char -> Int
letraANatural letra = ord letra - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar letra num
    |not (esMinuscula letra) = letra
    |otherwise = chr (97 + mod (letraANatural letra + num) 26)

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) num = desplazar x num : cifrar xs num

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (x:xs) num = desplazar x (-num) : descifrar xs num

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista lista = cifrarListaAux lista  0  (length lista)

cifrarListaAux :: [String] -> Int -> Int -> [String]
cifrarListaAux [] _ _ = []
cifrarListaAux (l:ls) desde hasta
    | desde < hasta = cifrar l desde: cifrarListaAux ls  (desde+1) hasta

-- EJ 7
frecuencia :: String -> [Float]
frecuencia frase = frecuenciaAux frase 0 25

-- Crear una lista de 26 elementos donde cada posicion representa una letra del abecedario y las veces que aparece cada letra en la frase
frecuenciaAux :: String -> Int -> Int -> [Float]
frecuenciaAux frase desde hasta
    | desde == hasta + 1 = []
    | pertenece letra frase = (cuantasVecesAparece letra frase)/ (contarMinusculas frase)*100 : frecuenciaAux frase (desde + 1) hasta
    | otherwise = 0.0 : frecuenciaAux frase (desde + 1) hasta
    where
        letra = chr (97+desde)

--evalua si un elemento pertenece o no a la lista
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs)
    | e == x = True
    | otherwise = pertenece e xs

--dada una frase, devuelve la cantidad de minusculas en float para hacer la division 
contarMinusculas :: String -> Float
contarMinusculas [] = 0
contarMinusculas (x:xs)
    | esMinuscula x = 1 + contarMinusculas xs
    | otherwise = contarMinusculas xs


-- dado un elemento y una lista devuelve cuantas veces aparece en la lista
cuantasVecesAparece :: (Eq t) => t -> [t] -> Float
cuantasVecesAparece _ [] = 0
cuantasVecesAparece e (x:xs)
    | e == x = 1 + cuantasVecesAparece e xs
    | otherwise = cuantasVecesAparece e xs

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente frase num = (chr (97 + indice (maximo (frecuencia (cifrar frase num))) (frecuencia (cifrar frase num))), maximo (frecuencia (cifrar frase num)))

indice :: (Eq t) => t -> [t] -> Int
indice e (x:xs)
    | e == x = 0
    | otherwise = 1 + indice e xs

maximo :: (Ord t) => [t] -> t
maximo (x:xs)
    | xs == [] = x
    | x > head (xs) = maximo (x : tail (xs))
    | otherwise = maximo xs

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado frase1 frase2 = ciclo frase1 frase2 0

ciclo :: String -> String -> Int -> Bool
ciclo frase1 frase2 num 
    | frase1 == frase2 = True
    | num < 27 = ciclo frase1 (cifrar frase2 1) (num + 1)
    | otherwise = False

-- EJ 10 
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados lista = todosLosDescifradosRecursion lista 0

--Funcion auxiliar que ayuda a hacer el paso recursivo
todosLosDescifradosRecursion :: [String] -> Int -> [(String, String)]
todosLosDescifradosRecursion lista contador
    |contador == length lista = []
    |otherwise = descifradosATupla (head lista) listaDescifrados ++ todosLosDescifradosRecursion rotarLista (contador+1)
        where listaDescifrados = descifradosLista (head lista) lista
              rotarLista = tail lista ++ [head lista]

--Cuenta el número de pasos que tuvo que hacer cifrar en frase para llegar al descifrado
numeroDePasos :: String -> String -> Int
numeroDePasos frase cifrado
    |frase == cifrado = 0
    |otherwise = 1 + numeroDePasos frase (descifrar cifrado 1)

--Da todos los descifrados de una palabra, si el elemento en lista es igual a frase, lo salta (excepto si frase no es minuscula en ninguna letra)
descifradosLista :: String -> [String] -> [String]
descifradosLista _ [] = []
descifradosLista frase lista 
    |cualquierCifradoEsFrase = head lista : descifradosLista frase (tail lista)
    |esDescifrado frase (head lista) && mod pasos 26 /= 0 = head lista : descifradosLista frase (tail lista)
    |otherwise = descifradosLista frase (tail lista)
        where pasos = numeroDePasos frase (head lista)
              cualquierCifradoEsFrase = frase == cifrar (head lista) 1 && frase == cifrar (head lista) 2 --Si ambos son iguales, entonces todas lo son

--Toma la lista de descifrados de la frase y los junta con la frase en una tupla
descifradosATupla :: String -> [String] -> [(String, String)] 
descifradosATupla _ [] = []
descifradosATupla frase lista = (frase, head lista) : descifradosATupla frase (tail lista)

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave "" _ = ""
expandirClave frase num
    | length frase > num = eliminarExceso frase num
    | otherwise = expandirClave (frase ++ frase) num

-- Toma una palabra y recortar los últimos n caracteres
eliminarExceso :: String -> Int -> String 
eliminarExceso "" _ = ""
eliminarExceso frase num
    | num == 0 = []
    | otherwise = head frase : eliminarExceso (tail frase) (num - 1) 

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
    where primeraLetra = desplazar (head frase1) ((letraANatural (head frase2)) * (-1))
          descifrarSiguienteLetra = descifrarVigenere (tail frase1) (tail (expandirClave frase2 (length frase1)))

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado "" _ = ""
peorCifrado frase [x] = x
peorCifrado frase (x:y:claves) -- Compara la distancia de secuencia de la primera clave con la segunda
    | x == "a" || y == "a" = "a"
    | primeraClave <= segundaClave = peorCifrado frase (y : claves)
    | otherwise = peorCifrado frase (x : claves)
        where primeraClave = absoluto (distanciaSecuencias frase (cifrarVigenere frase x)) 
              segundaClave = absoluto (distanciaSecuencias frase (cifrarVigenere frase y))

-- Distancia desde frase1 hasta frase2
distanciaSecuencias :: String -> String -> Int 
distanciaSecuencias "" "" = 0
distanciaSecuencias frase1 frase2 = diferenciaHeads + distanciaSecuencias (tail frase1) (tail frase2)
    where diferenciaHeads = ((letraANatural (head frase1)) - (letraANatural (head frase2)))

-- Funcion para dar el valor absoluto de la distancia
absoluto :: Int -> Int
absoluto n
    | n >= 0 = n
    | otherwise = -n

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] [] _ = []
combinacionesVigenere [] _  _ = [] -- Caso base necesario para el paso recursivo 
combinacionesVigenere frases claves cifrado = combinacionesVigPalabra frase claves cifrado ++ combinacionesVigenere restoFrases claves cifrado
    where clave = head claves
          frase = head frases
          restoFrases = tail frases

-- combinacionesVigenere en una palabra, para poder hacer el paso recursivo
combinacionesVigPalabra :: String -> [String] -> String -> [(String, String)] 
combinacionesVigPalabra _ [] _ = []
combinacionesVigPalabra frase claves cifrado
    | cifrarVigenere frase (head claves) == cifrado = (frase, head claves) : combinacionesVigPalabra frase (tail claves) cifrado
    | otherwise = combinacionesVigPalabra frase (tail claves) cifrado
