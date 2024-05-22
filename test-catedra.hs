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
    expectlistProximity (frecuencia "TALLER") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], -- Para todo caracter que no sea letras minúsculas, su valor es 0.0
    expectlistProximity (frecuencia "az") [50.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,50.0]
    ]

testsEjcifradoMasFrecuente = test [
    cifradoMasFrecuente "taller" 3 ~?= ('o', 33.333336)
    ]

testsEjesDescifrado = test [
    esDescifrado "taller" "compu" ~?= False
    ]

testsEjtodosLosDescifrados = test [
    expectPermutacion (todosLosDescifrados ["compu", "frpsx", "mywza"]) [("compu", "frpsx"), ("frpsx", "compu")],
    todosLosDescifrados [("Hola")] ~?= [],
    todosLosDescifrados [("qwer"),("hola")] ~?= [],
    --arreglar la funcion para este caso
    expectPermutacion (todosLosDescifrados ["abc", "bcd","cde"]) [("abc", "bcd"), ("abc","cde"),("bcd","abc"),("cde","abc"),("bcd","cde"),("cde","bcd")]
    ]

testsEjexpandirClave = test [
    expandirClave "compu" 8 ~?= "compucom",
    "n es igual a la longitud de la frase" ~: expandirClave "naranja" 7 ~?= "naranja",
    "n == 1 (primera letra)" ~: expandirClave "laboratorio" 1 ~?= "l"
    ]

testsEjcifrarVigenere = test [
    cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
    "frase vacia y clave generica" ~: cifrarVigenere "" "aofk" ~?= "",
    "clave == a (devuelve la misma frase)" ~: cifrarVigenere "congruencia" "a" ~?= "congruencia"
    ]

testsEjdescifrarVigenere = test [
    descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    "frase vacia y clave generica" ~: descifrarVigenere "" "gjk" ~?= "",
    "clave == a (no desplaza)" ~: descifrarVigenere "palabra" "a" ~?= "palabra" 
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
    "Niguna hace match" ~:combinacionesVigenere["casa", "palabra"] ["asd", "fr"] "poke" ~?= [],
    "cifrado vacio" ~: combinacionesVigenere ["compu", "labor"] ["jk", "qwert"] "" ~?= [],    
  --  "varios elementos" ~: expectPermutacion(combinacionesVigenere["abc","bcd"] ["b","c"] "cde") [("abc","c"),("bcd","b")],
    "varios match con una clave" ~: expectPermutacion(combinacionesVigenere["gnkz", "ipmb"] ["b", "z"] "hola") [("gnkz","b"),("ipmb", "z")]

    ]
