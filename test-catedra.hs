import Test.HUnit
import Solucion
import Data.List
-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]

testsEjesMinuscula = test [
    "letra minuscula" ~: esMinuscula 'd' ~?= True,
    "letra mayuscula" ~: esMinuscula 'D' ~?= False,
    "caracter vacio (no mayuscula)" ~: esMinuscula ' ' ~?= False,
    "caracter especial" ~: esMinuscula ']' ~?= False
    ]

testsEjletraANatural = test [
    "Primera letra del abecedario" ~: letraANatural 'a' ~?= 0,
    "Ultima letra del abecedario" ~:letraANatural 'z' ~?= 25,
    "letra entre 'a' y 'z'" ~: letraANatural 'b' ~?= 1
    ]

testsEjdesplazar = test [
    "desplazo positivo sin salto" ~: desplazar 'a' 3 ~?= 'd',
    "desplazo positivo con salto" ~: desplazar 'z' 3 ~?= 'c',
    "desplazo negativo sin salto" ~: desplazar 'h' (-3) ~?= 'e',
    "desplazo negativo con salto" ~: desplazar 'a' (-2) ~?= 'y', 
    "letra recorre todo el abecedario" ~: desplazar 'a' 26 ~?= 'a',
    "letra recorre mas de 26" ~: desplazar 'c' 28 ~?= 'e',
    "letra recorre menos de (-26)" ~: desplazar 'z' (-27) ~?= 'y',
    "letra no se desplaza" ~: desplazar 'g' 0 ~?= 'g',
    "letra es mayuscula" ~: desplazar 'Y' 9 ~?= 'Y'
    ]

testsEjcifrar = test [
    cifrar "computacion" 3 ~?= "frpsxwdflrq",
    "frase tiene alguna mayuscula y caracter especial" ~: cifrar "Abc2z#" 2 ~?= "Ade2b#",
    "frase es completamente mayuscula" ~: cifrar "HOLA" 4 ~?= "HOLA",
    "frase vacia" ~: cifrar "" 69 ~?= "",
    "cant_pasos == 0" ~: cifrar "frase" 0 ~?= "frase",
    "cant_pasos > 26" ~: cifrar "hola" 28 ~?= "jqnc"
    ]

testsEjdescifrar = test [
    descifrar "frpsxwdflrq" 3 ~?= "computacion",
    "cifrado tiene mayuscula y caracteres especiales" ~: descifrar "Ade2b#" 2 ~?= "Abc2z#",
    "cifrado es completamente mayuscula" ~: descifrar "HOLA" 4 ~?= "HOLA",
    "cifrado vacio" ~: descifrar "" 69 ~?= "",
    "cant_pasos == 0" ~: descifrar "xyz" 0 ~?= "xyz",
    "cant_pasos > 26" ~: descifrar "jqnc" 28 ~?= "hola"
    ]

testsEjcifrarLista = test [
    cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"],
    "lista tiene un elemento" ~: cifrarLista ["hola"] ~?= ["hola"],
    "lista vacia" ~: cifrarLista [] ~?= [],
    "cifrarLista con caracteres especiales" ~: cifrarLista ["hOla", "ab4z", "bB/b"] ~?= ["hOla", "bc4a", "dB/d"],
    "lista no contiene caracteres min." ~: cifrarLista ["28391" , "&%?[:" , "=28)!?"] ~?= ["28391", "&%?[:", "=28)!?"]
    ]

testsEjfrecuencia = test [
    expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
    "s no tiene minusculas" ~: expectlistProximity (frecuencia "TALLER") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "Frecuencia tiene numeros enteros" ~: expectlistProximity (frecuencia "az") [50.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,50.0],
    "Frecuencia tiene racionales" ~: expectlistProximity (frecuencia "abc") [33.333333, 33.333333, 33.333333,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

testsEjcifradoMasFrecuente = test [
    cifradoMasFrecuente "taller" 3 ~?= ('o', 33.333336),
    "caso una sola letra" ~: cifradoMasFrecuente "a" 1 ~?= ('b', 100.0),
    "caso empate" ~: expectAny (cifradoMasFrecuente "aabb" 2) [('c', 50.0),('d', 50.0)],
    "caso empate con caracteres especiales" ~: expectAny (cifradoMasFrecuente "89aaDb%b" 1) [('b', 50.0),('c', 50.0)]
    ]

testsEjesDescifrado = test [
    esDescifrado "taller" "compu" ~?= False,
    "caso generico" ~: esDescifrado "xyz" "abc" ~?= True,
    "s2 es descifrado pero s1 es mayuscula" ~: esDescifrado "ABCE" "zabd" ~?= False,
    "s1 contiene alguna mayuscula y caracter especial" ~: esDescifrado "Ho_l4a" "Hv_s4h" ~?= True, -- Ver cifrar "Ho_l4a" 7
    "s1 y s2 vacios" ~: esDescifrado "" "" ~?= True
    ]

testsEjtodosLosDescifrados = test [
    expectPermutacion (todosLosDescifrados ["compu", "frpsx", "mywza"]) [("compu", "frpsx"), ("frpsx", "compu")],
    expectPermutacion (todosLosDescifrados ["Hola", "hola", "Hpmb"]) [("Hola","Hpmb"),("Hpmb","Hola")],
    "Solo hay un elemento y ningun paso cumple (n `mod` 26) == 0 " ~: todosLosDescifrados ["Hola"] ~?= [],
    "Ninguno se relaciona" ~: todosLosDescifrados ["qwerty", "hola", "pala" ] ~?= [],
    "Todos se relacionan" ~: expectPermutacion (todosLosDescifrados ["abc", "bcd","cde"]) [("abc", "bcd"), ("abc","cde"),("bcd","abc"),("cde","abc"),("bcd","cde"),("cde","bcd")],
    "Se relacionan consigo mismo" ~: expectPermutacion (todosLosDescifrados ["ABC", "8&!?=", "A&72"]) [("ABC", "ABC"), ("8&!?=","8&!?="), ("A&72","A&72")] -- Tienen varios n en cifrar tales que (n `mod` 26) == 0
    ]

testsEjexpandirClave = test [
    expandirClave "compu" 8 ~?= "compucom",
    "n es menor a la longitud de frase" ~: expandirClave "hola" 3 ~?= "hol",
    "n es el doble de longitud de frase" ~: expandirClave "sistemas" 16 ~?= "sistemassistemas"
    -- expandirClave "gris" 1 ~?= "g"
    ]

testsEjcifrarVigenere = test [
    cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
    "clave es 'a' (No cifra)" ~: cifrarVigenere "computacion" "a" ~?= "computacion",
    "frase no contiene minusculas (No cifra)" ~: cifrarVigenere "23?!" "ts" ~?= "23?!",
    "cifrado de por medio" ~: cifrarVigenere "computacion" "ab" ~?= "cpmquuadipn",
    "frase vacia" ~: cifrarVigenere "" "jkp" ~?= "",
    "frase contiene una minuscula" ~: cifrarVigenere "COMPUTaCION"  "b" ~?= "COMPUTbCION"
    ]

testsEjdescifrarVigenere = test [
    descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    "clave es 'a' (No descifra)" ~: descifrarVigenere "computacion" "a" ~?= "computacion",
    "frase no contiene minusculas (No descifra)" ~: descifrarVigenere "?¡9#" "rt" ~?= "?¡9#",
    "descifrado de por medio" ~: descifrarVigenere "cpmquuadipn" "ab" ~?= "computacion",
    "cifrado vacio" ~: descifrarVigenere "" "ty" ~?= "",
    "cifrado contiene una minuscula" ~: descifrarVigenere "COMPUTbCION" "b" ~?= "COMPUTaCION"
    ]

testsEjpeorCifrado = test [
    peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef",
    "|claves| == 1 con clave generica" ~: peorCifrado "casa" ["afk"] ~?= "afk",
    "'a' esta en claves (no desplaza)" ~: peorCifrado "computacion" ["ip", "asdef", "a", "ksy"] ~?= "a",
    "Hay empate en distancias" ~: expectAny (peorCifrado "frase" ["jklmn", "lkmnj", "nmlkj"]) ["jklmn", "lkmnj", "nmlkj"],
    "frase vacia (no hay peorCifrado)" ~: peorCifrado "" ["ip", "qwerty", "slm"] ~?= ""
    ]

testsEjcombinacionesVigenere = test [
    combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")],
    "msjs se relacionan con claves distintas" ~: expectPermutacion(combinacionesVigenere ["abc","cde"] ["d", "b"] "def") [("abc","d"),("cde","b")],
    "msjs y claves vacio con cifrado generico" ~: combinacionesVigenere [] [] "cifrado" ~?= []
    ]

-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)
