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
cifrarLista [x] = [x]
cifrarLista xs = invertirLista (cifroInverso xs)

cifroInverso :: [String] -> [String]
cifroInverso [] = []
cifroInverso [x] = [x]
cifroInverso (x:xs) = cifrar (ultimo xs) (length xs) : cifroInverso (sacarUltimo (x:xs))

invertirLista :: [String] -> [String]
invertirLista [] = []
invertirLista xs = ultimo xs : invertirLista (sacarUltimo xs)

sacarUltimo :: [String] -> [String]
sacarUltimo [x] = []
sacarUltimo (y:ys) = y : sacarUltimo ys

ultimo :: [String] -> String
ultimo [x] = x
ultimo (y:ys) = ultimo ys

-- EJ 7
frecuencia :: String -> [Float]
frecuencia "" = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
frecuencia frase
    |sonMayusc frase = plantilla
    |otherwise = frecuencia_Recursiva frase plantilla
    where plantilla = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

frecuencia_Recursiva :: String -> [Float] -> [Float] -- Hace la frecuencia del resto
frecuencia_Recursiva frase plantilla
    |elementosDistintosDe0 plantilla == cantLetrasDistintas frase = plantilla
    |not (esMinuscula (head frase)) = frecuencia_Recursiva (rotar frase) plantilla
    |elementosDistintosDe0 plantilla /= cantLetrasDistintas frase = frecuencia_Recursiva (rotar frase) (frecuencia_primero frase plantilla)


frecuencia_primero :: String -> [Float] -> [Float] -- Hace la frecuencia del primer elemento
frecuencia_primero "" plantilla = []
frecuencia_primero frase plantilla
    |not (esMinuscula (head frase)) = frecuencia_primero (rotar frase) plantilla
    |otherwise = insertaEnLista porcentajeLetra (letraANatural (head frase)) plantilla
            where porcentajeLetra = calcularPorcentajeLetra (head frase) frase

sonMayusc :: String -> Bool
sonMayusc [] = True
sonMayusc (x:xs)
    |not (esMinuscula x) = sonMayusc xs
    |length (x:xs) > 0 = False


rotar :: String -> String
rotar (x:xs) = ultimaLetra xs : sacarUltimaLetra (x:xs)

sacarUltimaLetra :: String -> String
sacarUltimaLetra [x] = ""
sacarUltimaLetra (y:ys) = y : sacarUltimaLetra ys

ultimaLetra :: String -> Char
ultimaLetra "" = ' '
ultimaLetra [x] = head [x]
ultimaLetra (y:ys) = ultimaLetra ys

cantLetrasDistintas :: String -> Int -- Cuenta La cantidad de letras minusculas distintas que hay 
cantLetrasDistintas "" = 0
cantLetrasDistintas [x] = 1
cantLetrasDistintas (x:y:xs)
    |not (esMinuscula x) = cantLetrasDistintas (eliminarLetra x (x:y:xs))
    |x == y || x /= y = 1 + cantLetrasDistintas (eliminarLetra x (x:y:xs))


elementosDistintosDe0 :: [Float] -> Int
elementosDistintosDe0 [] = 0
elementosDistintosDe0 (x:xs)
    |x /= 0 = 1 + elementosDistintosDe0 xs
    |otherwise = elementosDistintosDe0 xs

calcularPorcentajeLetra :: Char -> String -> Float -- Calcula el porcentaje de la cantidad de una letra dentro de una frase
calcularPorcentajeLetra _ "" = 0.0
calcularPorcentajeLetra letra frase
    |esMinuscula (head frase) = ((contarLetras letra frase) / cantidadLista frase)*100

insertaEnLista :: t -> Int -> [t] -> [t] -- Dado un elemento y un número, lo inserta en esa posición (De izq a derecha) en una lista de 26 elementos, espacio vacío es 0
insertaEnLista _ _ [] = []
insertaEnLista elem pos lista
    |pos > 0 = head (lista) : insertaEnLista elem (pos - 1) (tail lista)
    |pos == 0 = elem : (tail lista)

cantidadLista :: String -> Float
cantidadLista [] = 0
cantidadLista (x:xs)
    |esMinuscula x = 1 + cantidadLista xs
    |otherwise = cantidadLista xs

contarLetras :: Char -> String -> Float
contarLetras _ "" = 0
contarLetras letra frase
    |letra == head frase = 1 + contarLetras letra (tail frase)
    |otherwise = contarLetras letra (tail frase)

eliminarLetra :: Char -> String -> String
eliminarLetra _ "" = ""
eliminarLetra letra frase
    |letra == head frase = eliminarLetra letra (tail frase)
    |otherwise = head frase : eliminarLetra letra (tail frase)

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente "" _ = (' ', 0)
cifradoMasFrecuente frase n = (letraMasRepetida fraseCifrada, mayorNumeroLista (frecuencia (fraseCifrada)))
    where fraseCifrada = cifrar frase n

mayorNumeroLista :: [Float] -> Float
mayorNumeroLista [x] = x
mayorNumeroLista (x:y:xs)
    |x >= y = mayorNumeroLista (x:xs)
    |otherwise = mayorNumeroLista (y:xs) 

letraMasRepetida :: String -> Char
letraMasRepetida "" = ' '
letraMasRepetida [x] = x
letraMasRepetida (x:y:xs)
    |cantLetrasDistintas (x:y:xs) == 1 = x
    |contarLetras x (x:y:xs) >= contarLetras (sigLetra) (x:y:xs) = letraMasRepetida (sacarMayuscula(eliminarLetra sigLetra (x:y:xs)))
    |otherwise = letraMasRepetida (sacarMayuscula (eliminarLetra x (x:y:xs)))
        where sigLetra = siguienteLetraDistinta (x:y:xs)

sacarMayuscula :: String -> String
sacarMayuscula "" = ""
sacarMayuscula frase
    |ord (head frase) >= 97 && ord (head frase) <= 122 = head frase : sacarMayuscula (tail frase)
    |otherwise = sacarMayuscula (tail frase) 

siguienteLetraDistinta :: String -> Char
siguienteLetraDistinta "" = ' '
siguienteLetraDistinta (x:y:xs)
    |x == y = siguienteLetraDistinta (y:xs)
    |otherwise = y

-- EJ 9
esDescifrado :: String -> String -> Bool
--esDescifrado "" "" = True
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

distanciaSecuencias :: String -> String -> Int -- Distancia desde frase1 hasta frase2
distanciaSecuencias "" "" = 0
distanciaSecuencias frase1 frase2 = diferenciaHeads + distanciaSecuencias (tail frase1) (tail frase2)
    where diferenciaHeads = ((letraANatural (head frase1)) - (letraANatural (head frase2)))

absoluto :: Int -> Int -- funcion para dar el valor absoluto de la distancia
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

combinacionesVigPalabra :: String -> [String] -> String -> [(String, String)] -- combinacionesVigenere en una palabra, para poder hacer el paso recursivo
combinacionesVigPalabra _ [] _ = []
combinacionesVigPalabra frase claves cifrado
    |cifrarVigenere frase (head claves) == cifrado = (frase, head claves) : combinacionesVigPalabra frase (tail claves) cifrado
    |otherwise = combinacionesVigPalabra frase (tail claves) cifrado
