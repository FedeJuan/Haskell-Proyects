-- funcion que cuenta la cantidad de particiones que tiene un numero
respuesta :: Int -> Int
respuesta n = length(particiones n)

-- funcion que muestra todas las particiones que tiene un numero.
particiones :: Int -> [[Int]]
particiones 1 = [[1]]
particiones n = [n]:(conjuntos 1 (n-1) 0 n)

-- funcion que toma un numero n y devuelve un conjunto formado por el resultado de meter el numero m en las particiones de
-- n-1 , y luego, utilizando la recursión, va metiendo distintos m en particiones de n que no contengan numeros mas chicos
-- que m.
conjuntos :: Int -> Int -> Int -> Int -> [[Int]]
conjuntos m 0 o c = []
conjuntos m n 0 c = metecjto (meter m (particiones n)) (conjuntos (m+1) (n-1) 1 c)
conjuntos m n o c | m > (div c 2) = []
                  | otherwise = metecjto (meter m (conjuntosin (particiones n) o m)) (conjuntos (m+1) (n-1) (o+1) c)

conjuntosin :: [[Int]] -> Int -> Int -> [[Int]]
conjuntosin [] o c = []
conjuntosin (x:xs) o c | o == c = x:(conjuntosin xs 1 c)
                       | elem o x = conjuntosin xs 1 c
                       | otherwise = conjuntosin (x:xs) (o+1) c

meter :: Int -> [[Int]] -> [[Int]]
meter a [] = []
meter a (x:xs) = (a:x):(meter a xs)

metecjto :: [[Int]] -> [[Int]] -> [[Int]]
metecjto [] b = b
metecjto a b = metecjto (tail a) ((head a):b)
