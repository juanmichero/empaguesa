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
    esMinuscula 'd' ~?= True,
    esMinuscula 'D' ~?= False,
    esMinuscula ' ' ~?= False,
    esMinuscula ']' ~?= False
    ]

testsEjletraANatural = test [
    letraANatural 'b' ~?= 1,
    letraANatural 'a' ~?= 0,
    letraANatural 'z' ~?= 25
    ]

testsEjdesplazar = test [
    desplazar 'a' 3 ~?= 'd',
    desplazar 'Y' 9 ~?= 'Y',
    desplazar 'a' 3 ~?= 'd',
    desplazar 'z' 1 ~?= 'a',
    desplazar 'a' (-1) ~?= 'z', 
    desplazar 'z' 5 ~?= 'e',
    desplazar 'a' 27 ~?= 'b',
    desplazar 'f' 26 ~?= 'f',
    desplazar 'g' 0 ~?= 'g',
    desplazar 'g' (-28) ~?= 'e'
    ]

testsEjcifrar = test [
    cifrar "computacion" 3 ~?= "frpsxwdflrq",
    cifrar "Abcz" 2 ~?= "Adeb",
    cifrar "HOLA" 4 ~?= "HOLA",
    cifrar " " 69 ~?= " ",
    cifrar "xyz" 0 ~?= "xyz"
    ]

testsEjdescifrar = test [
    descifrar "frpsxwdflrq" 3 ~?= "computacion",
    descifrar "Adeb" 2 ~?= "Abcz",
    descifrar "HOLA" 4 ~?= "HOLA",
    descifrar " " 69 ~?= " ",
    descifrar "xyz" 0 ~?= "xyz"
    ]

testsEjcifrarLista = test [
    cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"],
    cifrarLista ["hola"] ~?= ["hola"],
    cifrarLista [] ~?= [],
    cifrarLista ["hOla", "abz", "bBb"] ~?= ["hOla", "bca", "dBd"] 
    ]

testsEjfrecuencia = test [
    expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "TALLER") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "az") [50.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,50.0]
    ]

testsEjcifradoMasFrecuente = test [
    cifradoMasFrecuente "taller" 3 ~?= ('o', 33.333336),
    "caso una sola letra" ~: cifradoMasFrecuente "a" 1 ~?= ('b', 100.0),
    "caso empate" ~: expectAny (cifradoMasFrecuente "aabb" 2) [('c', 50.0),('d', 50.0)],
    "caso empate con caracteres especiales" ~: expectAny (cifradoMasFrecuente "aaDbb" 1) [('b', 50.0),('c', 50.0)]
    ]

testsEjesDescifrado = test [
    esDescifrado "taller" "compu" ~?= False,
    esDescifrado "ABCE" "ZABD" ~?= False,
    esDescifrado "ABCE" "zabd" ~?= False,
    esDescifrado "Hola" "Hvsh" ~?= True,
    esDescifrado "xyz" "abc" ~?= True,
    esDescifrado "xyz" "ab" ~?= False,
    esDescifrado "" "" ~?= True,
    esDescifrado "a8d9" "y8b9" ~?= True
    ]

testsEjtodosLosDescifrados = test [
    expectPermutacion (todosLosDescifrados ["compu", "frpsx", "mywza"]) [("compu", "frpsx"), ("frpsx", "compu")],
    expectPermutacion (todosLosDescifrados ["Hola", "Hpmb", "hola"]) [("Hola","Hpmb"),("Hpmb","Hola")],
    todosLosDescifrados [("Hola")] ~?= [],
    todosLosDescifrados [("qwer"),("hola")] ~?= [],
    expectPermutacion (todosLosDescifrados ["abc", "bcd","cde"]) [("abc", "bcd"), ("abc","cde"),("bcd","abc"),("cde","abc"),("bcd","cde"),("cde","bcd")],
    expectPermutacion (todosLosDescifrados ["abc","bc","cde"]) [("abc", "cde"),("cde","abc")]
    ]

testsEjexpandirClave = test [
    expandirClave "compu" 8 ~?= "compucom",
    expandirClave "hola" 3 ~?= "hol",
    expandirClave "sistemas" 17 ~?= "sistemassistemass",
    expandirClave "gris" 1 ~?= "g"
    ]

testsEjcifrarVigenere = test [
    cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
    cifrarVigenere "computacion" "a" ~?= "computacion",
    cifrarVigenere "computacion" "aaaaaaaa" ~?= "computacion",
    cifrarVigenere "computacion" "ab" ~?= "cpmquuadipn",
    cifrarVigenere "computacion" "abababababa" ~?= "cpmquuadipn",
    cifrarVigenere "" "abababababa" ~?= "",
    cifrarVigenere "COMPUTaCION"  "b" ~?= "COMPUTbCION"
    ]

testsEjdescifrarVigenere = test [
    descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    descifrarVigenere "computacion" "a" ~?= "computacion",
    descifrarVigenere "computacion" "aaaaaaaa" ~?= "computacion",
    descifrarVigenere "cpmquuadipn" "ab" ~?= "computacion",
    descifrarVigenere "cpmquuadipn" "abababababa" ~?= "computacion",
    descifrarVigenere "" "abababababa" ~?= "",
    descifrarVigenere "COMPUTbCION" "b" ~?= "COMPUTaCION"
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
    expectPermutacion(combinacionesVigenere ["abc","cde"] ["d", "b"] "def") [("abc","d"),("cde","b")],
    combinacionesVigenere ["abc", "cde"] ["ab","bc"]  "acc" ~?= [("abc","ab")]
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
