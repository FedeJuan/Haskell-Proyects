-- funcion que recibe un numero y devuelve la cantidad de particiones que tiene un numero.
respuesta :: Int -> Int
respuesta n  | n < 2 = 0
             | otherwise = length(conjuntoreal(listasumas n))

-- funcion que recibe un set de conjuntos de numeros y elimina los conjuntos repetidos, dejando el que corresponde.
conjuntoreal :: [[Int]] -> [[Int]]
conjuntoreal [] = []
conjuntoreal (x:xs) | verifica x xs = conjuntoreal xs
                    | otherwise = x:(conjuntoreal xs)

-- funcion que recibe un conjunto y un set, y devuelve True si ese conjunto está dentro del set, o False si no está.
verifica :: [Int] -> [[Int]] -> Bool
verifica x [] = False
verifica x xs | x == (head xs) = True
              | otherwise = verifica x (tail xs)

-- funcion que recibe un numero y devuelve un set que contiene conjuntos de numeros que si sumas entre ellos, el 
-- resultado da el numero ingresado por el usuario.
listasumas :: Int -> [[Int]]
listasumas 2 = [[1,1]]
listasumas n = metecjto (irmetiendo 1 (n-1)) (sumas (n-1) 1 n)

-- funcion que recibe dos sets de conjuntos y devuelve un set con todos los conjuntos de ambos sets.
metecjto :: [[Int]] -> [[Int]] -> [[Int]]
metecjto [] b = b
metecjto a b = metecjto (tail a) ((head a):b)

irmetiendo :: Int -> Int -> [[Int]]
irmetiendo a b | b == 1 = []
               | otherwise = (metecjto (ccordenado(meter a (listasumas b))) (irmetiendo (a+1) (b-1)))

conjuntoordenado :: [Int] -> [Int]
conjuntoordenado [] = []
conjuntoordenado (x:xs) | xs == [] = [x]
                        | x > head xs = (head xs):(conjuntoordenado (x:(tail xs)))
                        | otherwise = (x:xs)
                        
ccordenado :: [[Int]] -> [[Int]]
ccordenado [] = []
ccordenado (x:xs) = (conjuntoordenado x):(ccordenado xs)

sumas :: Int -> Int -> Int -> [[Int]]
sumas a b n | (a < n `quot` 2) || ([a,b] == [(b-1),(a+1)]) = []
            | otherwise = (conjuntoordenado [a,b]):(sumas (a-1) (b+1) n)

-- funcion que recibe un numero y un set de conjuntos, y devuelve ese set con el elemento adentro de cada
-- conjunto perteneciente al set.
meter :: Int -> [[Int]] -> [[Int]]
meter a [] = []
meter a (x:xs) = (a:x):(meter a xs)
