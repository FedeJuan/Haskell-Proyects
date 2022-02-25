-- funciones que reciben un numero natural n y devuelve True si y solo si el n es par, mayor que 2 y suma de dos numeros primos.
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n | mod n 2 == 0 && n > 2 && sumaDeDosPrimos n 2 == n = True
                    | otherwise = False

sumaDeDosPrimos :: Integer -> Integer -> Integer
sumaDeDosPrimos n a | (n-a) <= 1 = 0
                    | 0 == funcionPrimos a a = sumaDeDosPrimos n (a+1)
                    | 0 == funcionPrimos (n-a) (n-a) = sumaDeDosPrimos n (a+1)
                    | otherwise = n

funcionPrimos :: Integer -> Integer -> Integer
funcionPrimos 2 y = 2 
funcionPrimos x 2 = x
funcionPrimos x y | mod x (y-1) == 0 = 0
                  | otherwise = funcionPrimos x (y-1)

-- funcion que reciben el numero y devuelve True si se cumple la conjetura para todos los numeros pares menores que ese numero.
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n | n == 4 = True
                          | satisfaceGoldbach n = verificarConjeturaHasta (n-2)
                          | otherwise = False

-- funcion que recibe un numero y devuelve un par ordenado de numeros primos cuya suma da ese numero.
descomposicionEnPrimos :: Integer -> (Integer, Integer)
descomposicionEnPrimos n | satisfaceGoldbach n = descomposicionEnPrimosDesde n 2

descomposicionEnPrimosDesde :: Integer -> Integer -> (Integer, Integer)
descomposicionEnPrimosDesde n d | (n-d) == 1 = (0,0)
                                | 0 == funcionPrimos d d = descomposicionEnPrimosDesde n (d+1)
                                | 0 == funcionPrimos (n-d) (n-d) = descomposicionEnPrimosDesde n (d+1)
                                | otherwise = (d,n-d)

-- funciones que reciben un numero natural n par y devuelven la cantidad de pares ordenados de primos que suman ese numero.
numeroDeDescomposiciones :: Integer -> Integer 
numeroDeDescomposiciones n = contadorDescomposiciones n 2

contadorDescomposiciones :: Integer -> Integer -> Integer
contadorDescomposiciones n k | n - k <= 1 = 0
                             | 0 == funcionPrimos k k || 0 == funcionPrimos (n-k) (n-k) = 0 + contadorDescomposiciones n (k+1)
                             | otherwise = 1 + contadorDescomposiciones n (k+1)
