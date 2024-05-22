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
pertenece ::(Eq t)=>t -> [t] -> Bool
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
cuantasVecesAparece ::(Eq t) => t -> [t] -> Float
cuantasVecesAparece _ [] = 0
cuantasVecesAparece e (x:xs)
    | e == x = 1 + cuantasVecesAparece e xs
    | otherwise = cuantasVecesAparece e xs

-- Ej 8
cifradoMasFrecuente:: String -> Int -> (Char,Float)
cifradoMasFrecuente frase n = (chr (97 + indice (maximo (frecuencia (cifrar frase n))) (frecuencia (cifrar frase n))), maximo (frecuencia (cifrar frase n)))

indice :: (Eq t)=> t ->[t]-> Int
indice e (x:xs)
    | e == x = 0
    | otherwise = 1 + indice e xs

maximo ::(Ord t)=> [t] -> t
maximo (x:xs)
    |xs ==[] = x
    |x > head (xs) = maximo (x:tail (xs))
    |otherwise = maximo xs

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado frase1 frase2 = ciclo frase1 frase2 0

ciclo :: String -> String -> Int -> Bool
ciclo frase1 frase2 n 
    |frase1 == frase2 = True
    |n < 27 = ciclo frase1 (cifrar frase2 1) (n+1)
    |otherwise = False

-- EJ 10 
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados lista = eliminarRepetidos (descifradosLista lista 0)

-- Elimina los posibles repetidos de a funcion auxiliar de abajo
eliminarRepetidos:: (Eq t) => [t] -> [t] 
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
    |elem x xs = eliminarRepetidos xs
    |otherwise = x : eliminarRepetidos xs

-- Recorre la lista y devuelve todos los descifrados (Puede haber repetidos) 
descifradosLista :: [String] -> Int -> [(String, String)]
descifradosLista lista n 
    |n == (length lista) = []
    |cifradoEnLista (head lista) (tail lista) = (tuplaCifrado : tuplaCifradoInv : descifradosLista rotarLista (n+1))
    |otherwise = descifradosLista rotarLista (n+1)
        where tuplaCifrado = (head lista, cifrado (head lista) (tail lista))
              tuplaCifradoInv = (cifrado (head lista) (tail lista), head lista)
              rotarLista = (tail lista) ++ [(head lista)]

-- Dado un String y una lista, da el cifrado o descifrado de cualquiera de sus pasos, de no haberlo, da un espacio en blanco
cifrado :: String -> [String] -> String 
cifrado frase [] = ""
cifrado frase lista
    |esDescifrado frase (head lista) = head lista
    |otherwise = cifrado frase (tail lista)

-- Verifica que el cifrado o descifrado de una palabra esté dentro de la lista 
cifradoEnLista :: String -> [String] -> Bool 
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

-- Toma una palabra y recortar los últimos n caracteres
eliminarExceso :: String -> Int -> String 
eliminarExceso "" _ = ""
eliminarExceso frase n
    |n == 0 = []
    |otherwise = head frase : eliminarExceso (tail frase) (n-1) 

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere ""  ""   = []
cifrarVigenere ""  _    = ""
cifrarVigenere frase "" = frase
cifrarVigenere frase1 frase2 = primeraLetra : cifrarSiguienteLetra
    where primeraLetra = desplazar (head frase1) (letraANatural(head frase2))
          cifrarSiguienteLetra = cifrarVigenere (tail frase1) (tail (expandirClave frase2 (length frase1)))

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere "" ""    = []
descifrarVigenere "" _     = ""
descifrarVigenere frase "" = frase
descifrarVigenere frase1 frase2 = primeraLetra : descifrarSiguienteLetra
    where primeraLetra = desplazar (head frase1) ((letraANatural(head frase2))*(-1))
          descifrarSiguienteLetra = descifrarVigenere (tail frase1) (tail (expandirClave frase2 (length frase1)))

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado "" _ = ""
peorCifrado frase [x] = x
peorCifrado frase (x:y:claves) -- Compara la distancia de secuencia de la primera clave con la segunda
    |x == "a" || y == "a" = "a"
    |primeraClave <= segundaClave = peorCifrado frase (y:claves)
    |otherwise = peorCifrado frase (x:claves)
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
    |n >= 0 = n
    |otherwise = -n

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
    |cifrarVigenere frase (head claves) == cifrado = (frase, head claves) : combinacionesVigPalabra frase (tail claves) cifrado
    |otherwise = combinacionesVigPalabra frase (tail claves) cifrado