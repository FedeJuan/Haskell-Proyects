type Posicion = [Int] -- cantidad de piedras que hay en cada montoncito.
type Jugada = (Int, Int) -- posicion del montoncito (x) empezando en 1 y cantidad que desea extraer (c).

-- Función del Ejercicio 1.
-- Define los tipos en la primer linea, en la segunda ataja el conjunto vacio luego de que termine la recursion asi la funcion no queda en un bucle.
-- La tercer linea comienza la recursion y filtra el montocito que quedo sin piedras sacandolo del conjunto. La cuarta extrae la cantidad indicada
-- por el usuario del montoncito y continua la recursion. La quinta se encarga de hacer la recursion para los montoncitos en lo que no hay que modificar.

jugar :: Posicion -> Jugada -> Posicion
jugar [] (x,c) = []
jugar (y:ys) (x,c) | y == 0 = ys
jugar (y:ys) (1,c) = jugar ((y-c):ys) (0,c)
jugar (y:ys) (x,c) = y:(jugar ys ((x-1),c)) 

-- Funciones del Ejercicio 2.
-- La primer funcion verifica el conjunto vacio y llama a la segunda funcion, que utiliza 2 parametros para escribir conjuntos (c,y) del primer
-- elemento head, si es que hay piedritas, hasta que se queda sin piedritas.  Una vez que cuenta todos del primer elemento, pasa al siguiente
-- head del conjunto tail, aumentando en 1 el contador.

posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas (y:ys) = conjuntosDePosicion (y:ys) 1

conjuntosDePosicion :: Posicion -> Int -> [Jugada]
conjuntosDePosicion [] _ = []
conjuntosDePosicion (0:ys) c = conjuntosDePosicion ys (c+1)
conjuntosDePosicion (y:ys) c = ((c,y):(conjuntosDePosicion ((y-1):ys) c))

-- Función del Ejercicio 3
-- Utiliza la funcion del ejercicio 4, si la posicion solo tiene jugadas perdedoras, o sea (0,0) o conjunto vacio, tira un false, sino tira True.

esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p | jugadaGanadora p == (0,0) || p == [] = False
                     | otherwise = True

-- Función del Ejercicio 4
-- Esta funcion se encarga de filtrar el conjunto vacio arrojando un (0,0), que significa que la posicion indicada es perdedora.
-- Luego, utiliza una funcion auxiliar especifica que se encarga de tomar una posicion y un conjunto de jugadas posibles (ejercicio 2) dentro de
-- una posicion sin elementos repetidos. Detallo la explicacion en funciones auxiliares.
-- Esa funcion auxiliar utiliza la funcion del ej 1 para probar si la primer jugada del conjunto de posibles jugadas,  deja al rival en una
-- posicion ganadora o perdedora, si lo deja en una ganadora, continua con otra jugada. Si lo deja en una perdedora, arroja como resultado de
-- jugada ganadora esa jugada. Si todas las jugadas fueron probadas y ninguna resultó ganadora, se devuelve un (0,0), porque es posicion perdedora.

jugadaGanadora :: Posicion -> Jugada
jugadaGanadora [] = (0,0)
jugadaGanadora (y:ys) = devuelvePrimerJugadaGanadora (y:ys) (posiblesJugadas (posicionSinElementosRepetidos (y:ys)))

devuelvePrimerJugadaGanadora :: Posicion -> [Jugada] -> Jugada
devuelvePrimerJugadaGanadora (y:ys) [] = (0,0)
devuelvePrimerJugadaGanadora (y:ys) (x:xs) | jugadaGanadora(jugar (y:ys) x) /= (0,0) = devuelvePrimerJugadaGanadora (y:ys) xs
                                           | otherwise = x

-- Funcion del Ejercicio 5.
-- Esta funcion recibe una posicion, en la cual utiliza la funcion del ejercicio 4 para verificar que sea perdedora. Si no lo es, utiliza una
-- funcion auxiliar que arroje el numero de jugadas ganadoras dentro de todas las jugadas posibles.

numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p | jugadaGanadora p == (0,0) = 0
numeroDeJugadasGanadoras (y:ys) = conjuntoDeJugadasGanadoras (y:ys) (posiblesJugadas (y:ys))

-- funcion auxiliar que agarra una posicion y un conjunto de jugadas posibles y lo reduce a un conjunto de jugadas que son ganadoras, utiliza
-- la funcion 'jugar' para probar si la posicion resultante de usar esa jugada es perdedora. Si lo es, se le suma al conjunto de jugadas
-- una unidad, y prosigue con las demas jugadas posibles. Una vez que se agota, o sea, que llega al conjunto vacio, arroja el numero final de
-- jugadas ganadoras.

conjuntoDeJugadasGanadoras :: Posicion -> [Jugada] -> Int
conjuntoDeJugadasGanadoras (y:ys) [] = 0
conjuntoDeJugadasGanadoras (y:ys) (x:xs) | jugadaGanadora(jugar (y:ys) x) /= (0,0) = conjuntoDeJugadasGanadoras (y:ys) xs
                                         | otherwise = 1 + conjuntoDeJugadasGanadoras (y:ys) xs


--                                    FUNCIONES AUXILIARES

-- Esta funcion agarra una posicion y reemplaza los elementos repetidos con un cero, salvo el ultimo, lo que hace que el conjunto de jugadas posibles
-- sea mas acotado ya que no va a tener en cuenta las jugadas "repetidas" que llevan al mismo resultado. Por ejemplo en jugadasPosibles
-- de [1,1,2,2,1] es lo mismo jugar (1,1), (2,1) o (5,1), y tambien es lo mismo jugar (3,1) y (4,1) o (3,2) y (4,2). Por eso al analizar las
-- jugadas posibles de [0,0,0,2,1], que son solo tres, la funcion del ej 3 y 4 va a tardar muchisimo menos en encontrar una jugada ganadora.
-- TENER EN CUENTA que yo no estoy cambiando el resultado de [1,1] por [0,1] ya que una es posicion perdedora y la otra no, lo que estoy 
-- haciendo nada mas es acotar el conjunto de jugadas posibles a uno mas optimo, el resultado es el mismo pero va a ser alcanzado mas rapido.
-- Logicamente esta funcion no es utilizada en el ejercicio 5 ya que ahi si hay que tener en cuenta TODAS las jugadas posibles, incluso las repetidas.

posicionSinElementosRepetidos:: Posicion -> Posicion
posicionSinElementosRepetidos (y:[]) = [y]
posicionSinElementosRepetidos (y:ys) | pertenece y ys = (0:(posicionSinElementosRepetidos ys))
                            | otherwise = (y:(posicionSinElementosRepetidos ys))

-- funcion que verifica si el elemento x forma parte de un conjunto (y:ys), si pertenece a ese conjunto devuelve True, sino False.
pertenece :: Int -> Posicion -> Bool
pertenece x [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys
