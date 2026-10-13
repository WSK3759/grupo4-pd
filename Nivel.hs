import Test.QuickCheck

-- Tipos sinónimos
type Vector2D = (Double, Double)
type CajaColision = (Double, Double, Double, Double) -- Posición (x, y), Ancho y Alto
type Celda = Char
type Grid = [String] -- String de caracteres Celda, grid = [[Celda]]

-- 1 Vectores 2D

sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores v1 v2 = ((fst v1 + fst v2),(snd v1 + snd v2))

escalarVector :: Double -> Vector2D -> Vector2D
escalarVector x v = ((x * fst v),(x * snd v))

distancia :: Vector2D -> Vector2D -> Double
distancia v1 v2 = sqrt((fst v1 - fst v2)^2 + (snd v1 - snd v2)^2)

-- 2 Cajas de colisión

solapan :: CajaColision -> CajaColision -> Bool
solapan (x1,y1,ancho1,alto1) (x2,y2,ancho2,alto2) =
    x1 + ancho1 > x2 &&
    x2 + ancho2 > x1 &&
    y1 + alto1 > y2 &&
    y2 + alto2 > y1

-- 3 Lado de Colisión

ladoColision :: CajaColision -> CajaColision -> String
ladoColision (x1, y1, a1, h1) (x2, y2, a2, h2)
    | arriba <= abajo && arriba <= izquierda && arriba <= derecha = "Arriba"
    | abajo <= arriba && abajo <= izquierda && abajo <= derecha = "Abajo"
    | izquierda <= arriba && izquierda <= abajo && izquierda <= derecha = "Izquierda"
    | otherwise = "Derecha"
    where
        arriba    = (y2 + h2) - y1
        abajo     = (y1 + h1) - y2
        izquierda = (x1 + a1) - x2
        derecha   = (x2 + a2) - x1

-- 4 UTILIDADES DE LISTAS Y CADENAS

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
    | c == x    = "" : lista
    | otherwise = (x : head lista) : tail lista
    where
        lista = splitOn c xs

trim :: String -> String
trim xs = reverse (dropWhile esEspacio (reverse (dropWhile esEspacio xs)))
    where
        esEspacio c = c `elem` [' ', '\t', '\n']

contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple p xs = length [x | x <- xs, p x]

list2Vector2 :: [Double] -> Vector2D
list2Vector2 [] = error "head : Lista vacia no posible convertir en Vector2"  
list2Vector2 [x]  = error "head : Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) = (x, y)

-- 5 PARSEO DE NIVEL

parsearNivel :: [String] -> Grid
parsearNivel s = s

-- Funcion: esSolido --> Indica si una celda es una plataforma sólida, identificando con pattern matching si es el caracter del convenio indicado
esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _ = False

-- Funcion: esMeta --> Indica si una celda es la meta del nivel, identificando con pattern matching si es el caracter del convenio indicado
esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _ = False

-- Funcion: esVacio --> Indica si una celda está vacia, identificando con pattern matching si es el caracter del convenio indicado
esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _ = False

-- Función: esEnemigo
-- Indica si una celda marca el punto de inicio de un enemigo (cualquier carácter que no sea sólido, meta ni vacío).
esEnemigo :: Celda -> Bool
esEnemigo celda = not (esSolido celda || esMeta celda || esVacio celda)

posicionesMeta :: Grid -> [(Int, Int)]
posicionesMeta nivel = [ (f, c)
  | (f, fila) <- zip [0..] nivel
  , (c, celda) <- zip [0..] fila
  , esMeta celda
  ]

posicionesEnemigos :: Grid -> [(Int, Int, Char)]
posicionesEnemigos nivel =
    [(indiceLista, indiceString, caracter)
    |(indiceLista, stringLista) <- zip [0..] nivel
    ,(indiceString, caracter) <- zip [0..] stringLista
    , esEnemigo caracter
    ]

-- Función: agruparRachas
agruparRachas :: String -> [(Int, Int)]
agruparRachas fila = aux 0 fila
  where
    aux :: Int -> String -> [(Int, Int)]
    aux _ "" = []
    aux indice xs
      | not (esSolido (head xs)) =
          let (_, resto) = span (not . esSolido) xs     -- separa los caracteres no sólidos del resto con "_" ignora la parte no solida y resto = parte de la fila que queda
              saltados   = length xs - length resto
          in aux (indice + saltados) resto       -- avanzamos el índice y seguimos buscando
      | otherwise =
          let (solidos, resto) = span esSolido xs   -- cogemos todos los caracteres sólidos consecutivos, resto = lo que queda después de la racha
              longitud = length solidos  -- lo que mide la racha
          in (indice, longitud) : aux (indice + longitud) resto -- Guardamos (posición inicial, longitud) y seguimos buscando más rachas después de esta

--EJERCICIO 6
-- Suma conmutativa
prop_suma_conmutativa :: Vector2D -> Vector2D -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a
-- +++ OK, passed 100 tests.

-- Sumar en un orden u otro da el mismo resultado
prop_suma_asociativa :: Vector2D -> Vector2D -> Vector2D -> Bool
prop_suma_asociativa a b c = sumaVectores (sumaVectores a b) c == sumaVectores a (sumaVectores b c)
--Failed! Falsified (after 9 tests and 12 shrinks):
--(0.6,0.0)
--(0.3,0.0)
--(4.0,0.0)
--Falla por pequeños errores acumulados al redondear

-- Escalar un vector por 1 no lo cambia
prop_escalar_neutro :: Vector2D -> Bool
prop_escalar_neutro v = escalarVector 1 v == v
-- +++ OK, passed 100 tests.

-- La distancia entre dos puntos nunca es negativa
prop_distancia_no_negativa :: Vector2D -> Vector2D -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0
-- +++ OK, passed 100 tests.

-- La distancia de a a b es igual que de b a a
prop_distancia_simetrica :: Vector2D -> Vector2D -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a
-- +++ OK, passed 100 tests.

-- solapan a b es igual a solapan b a
prop_solapan_simetrica :: CajaColision -> CajaColision -> Bool
prop_solapan_simetrica a b = solapan a b == solapan b a
-- +++ OK, passed 100 tests.

-- Aplicar trim dos veces da el mismo resultado que aplicarlo una vez
prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s
-- +++ OK, passed 100 tests.

-- Si el separador no aparece en la cadena, splitOn devuelve [cadena]
prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador c s = notElem c s ==> splitOn c s == [s]
-- +++ OK, passed 100 tests; 14 discarded.

-- El resultado de contarSiCumple nunca es mayor que la longitud de la lista
prop_contarSiCumple_acotado :: [Int] -> Bool
prop_contarSiCumple_acotado xs = contarSiCumple even xs <= length xs

-- * / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /

-- TAREA 2

-- BLOQUE 1 - RECURSION
-- 1 Reimplementación recursiva de las consultas del nivel 

-- Funcion: posicionesMetaRec
-- Dado el nivel completo, devuelve la lista de posiciones (fila, columna) en las que aparece la meta, recorriendo de forma recursiva las filas y las columnas, acumulando indice en cada llamada recursiva
-- IMPLEMENTACION: Miriam/Enrique

posicionesMetaRec :: Grid -> [(Int, Int)]
posicionesMetaRec nivel = filas 0 nivel -- recorrer nivel empezando por fila 0 "..M"
  where
    filas :: Int -> Grid -> [(Int, Int)]
    filas _ [] = []
    filas f (fila:resto) =
        columnas f 0 fila ++ filas (f+1) resto

    columnas :: Int -> Int -> String -> [(Int, Int)]
    columnas _ _ [] = []
    columnas f c (x:xs) -- fila,columna,caracterAct,restoCar
      | esMeta x = (f,c) : columnas f (c+1) xs -- guardamos posición y seguimos buscando
      | otherwise = columnas f (c+1) xs


-- Función: posicionesEnemigosRec -> igual que posicionesMetaRec, pero para las posiciones e identificadores de los enemigos
-- IMPLEMENTACION: Enrique

posicionesEnemigosRec :: Grid -> [(Int, Int, Char)] -- Devuelve la lista de posiciones e identificadores (fila, columna, carácter) de los enemigos mediante recursión
posicionesEnemigosRec nivel = auxFilas 0 nivel
  where
    auxFilas :: Int -> Grid -> [(Int, Int, Char)]
    auxFilas indiceFila [] = []
    auxFilas indiceFila (fila:filas) = 
        auxColumnas indiceFila 0 fila ++ auxFilas (indiceFila + 1) filas

    auxColumnas :: Int -> Int -> String -> [(Int, Int, Char)]
    auxColumnas _ _ [] = []
    auxColumnas f c (caracter:xs) 
        | esEnemigo caracter = (f, c, caracter) : auxColumnas f (c + 1) xs
        | otherwise = auxColumnas f (c + 1) xs

-- 2 Generalización por recursión de agruparRachas

-- Función: agruparPorCaracter -> Recorre una fila y 
-- agrupa en tríos (columna de inicio, longitud, carácter) las rachas de caracteres consecutivos iguales.
-- IMPLEMENTACION: Jesús

agruparPorCaracter :: [Celda] -> [(Int, Int, Celda)]
agruparPorCaracter "" = [] -- caso base: si la fila esta vacia, devuelve una lista vacia
agruparPorCaracter (chInicial:resto) = agruparPorCaracterRAux 0 1 chInicial resto 
-- caso recursivo: llamada a la funcion auxiliar con indice acumulador de columna, longitud y caracter actual al que se le pasa el resto de la fila ademas

agruparPorCaracterRAux :: Int -> Int -> Celda -> [Celda] -> [(Int, Int, Celda)] -- func auxiliar: lleva con un indice
agruparPorCaracterRAux col lng ch [] = [(col, lng, ch)]
agruparPorCaracterRAux col lng ch (celda:resto)
 | celda == ch =  agruparPorCaracterRAux col (lng+1) ch resto
 | otherwise = (col, lng, ch) : agruparPorCaracterRAux (col+lng) 1 celda resto


-- 3 Recursion Numerica: simulacion de movimiento

-- Función: simularCaidaLibre --> Dado un número de pasos, una aceleración (gravedad) 
-- y un estado inicial (posición, velocidad), calcula recursivamente 
-- la lista de todos los estados por los que pasa un objeto en caída libre 
-- con avance discreto
-- IMPLEMENTACION: Miriam

-- Auxiliar que pide el enunciado: calcula un solo paso de la caída libre.
soloUnPaso :: Double -> (Double, Double) -> (Double, Double)
soloUnPaso gravedad (posicion, velocidad) =
    (posicion + velocidad, velocidad + gravedad) -- pm

-- Simula la caída libre guardando el estado inicial y los siguientes estados.
simularCaidaLibre :: Int -> Double -> (Double, Double) -> [(Double, Double)]
simularCaidaLibre 0 _ estado = [estado] -- Si quedan 0 pasos, ya no calculamos nada más pero devolvemos el estado
simularCaidaLibre n gravedad estado =
    estado : -- guarda el estado actual en la lista
    simularCaidaLibre (n-1) -- vuelve a llamar a la misma función, pero con un paso menos
    gravedad -- la gravedad no cambia durante la simulación
    (soloUnPaso gravedad estado) -- calcula nuevo estado


-- 4 Recursión sobre dos listas a la vez (0.5 puntos)

-- Función: zipPropio --> 
    -- Empareja los elementos de dos listas, uno a uno, 
    -- deteniéndose en cuanto una de las dos se acaba
-- IMPLEMENTACION: Enrique

zipPropio :: [a] -> [b] -> [(a, b)] -- Empareja los elementos de dos listas uno a uno deteniéndose cuando se acaba alguna.
zipPropio [] _ = []
zipPropio _ [] = []
zipPropio (x:xs) (y:ys) = (x, y) : zipPropio xs ys

-- 5 Recursión con descarte condicional: preparación de líneas

-- Función: esComentarioOVacia --> Dada una línea (lista de caracteres), devuelve un Bool indicando si dicha linea es vacía (" ") o es un comentario (que empiezan por ';')
-- IMPLEMENTACION: Gonzalo
-- Determinar si una línea es un comentario o está vacía
esComentarioOVacia :: String -> Bool
esComentarioOVacia linea 
    | trim linea == ""           = True
    | head (trim linea)  == ';'  = True
    | otherwise                  = False

-- Función: prepararLineas --> Dada una lista de líneas de fichero de nivel, recorre la lista limpiando el formato: elimina los espacios vacios al principio y al final y las lineas con comentarios
-- IMPLEMENTACION: Gonzalo
--Aplicar la función anterior a un conjunto de líneas
prepararLineas :: [String] -> [String]
prepararLineas [] = []
prepararLineas (x:xs)
    | esComentarioOVacia x == True = prepararLineas xs
    | otherwise                    =  trim x : prepararLineas xs


-- Bloque 2: Funciones de orden superior
-- Subtarea 5: mapPropio y filterPropio

-- Función: mapPropio --> dada una funcion y una lista de tipo generico, devuelve la lista de resultados aplicando la funcion a cada elemento, utilizando orden superior
-- IMPLEMENTACION: Jesús
mapPropioR :: (a -> b) -> [a] -> [b] -- Utilizando patrones con recursion no terminal
mapPropioR _ [] = []
mapPropioR f (x:xs) = f x : mapPropioR f xs

-- Utilizando funciones orden superior:
mapPropio :: (a -> b) -> [a] -> [b]
mapPropio f = foldr auxMapPropio []
 where
    auxMapPropio x r = f x : r

-- Funcion: filterPropio --> Dado un predicado y una lista de elementos, devuelve la lista de los elementos que cumplen el predicado
-- IMPLEMENTACION: Enrique/Miriam/Gonzalo
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ [] = []
filterPropio cond (x:xs)
    | cond x    = x : filterPropio cond xs
    | otherwise = filterPropio cond xs


-- Subtarea 6: foldrPropio y foldlPropio
-- IMPLEMENTACION: Jesús/Miriam/Enrique/Gonzalo
-- Funcion: foldrPropio --> dado una funcion binaria, un elemento incial y una lista de elementos de distinto tipo, devuelve la combinacion de los elementos de la lista usando la funcion binaria
foldrPropio :: (a -> b -> b) -> b -> [a] -> b 
foldrPropio _ e [] = e
foldrPropio f e (x:xs) = f x (foldrPropio f e xs) -- llamada recursiva con el resto de la lista para combinar de derecha (final de lista) a izquierda (inicio)

-- Función: foldlPropio 
foldlPropio :: (b -> a -> b) -> b -> [a] -> b
foldlPropio _ ac []     = ac
foldlPropio f ac (x:xs) = foldlPropio f (f ac x) xs

-- Subtarea 7: Aplicación justificada de foldr/foldl

-- Funcion: hayColisionConAlguna --> Dada una cajaColision de un objeto, y una lista de cajasColision
-- indicar si existe colision con alguna de ellas
-- Debe implementarse con foldr
-- IMPLEMENTACION: Jesús/Gonzalo
-- Utilizo la funcion de la Tarea 1: `solapan`, para comprobar si colisionan en algun lado
hayColisionConAlguna :: CajaColision -> [CajaColision] -> Bool
hayColisionConAlguna cajaInicial = foldr auxCombinadora False
 where
    auxCombinadora caja r = solapan cajaInicial caja || r

-- En esta funcion utilizamos 'foldr' porque nos permite aprovechar la evaluación perezosa (lazy evaluation): 
-- al combinar las condiciones con el operador (||), en el momento en que 'solapan cajaInicial caja' devuelve True,
-- se deja de evaluar y se devuelve True inmediatamente sin evaluar el resto de cajas, ya que el operador (||) esta definido de forma perezosa por la izquierda.


-- Función: contarCeldasQueCumplen --> Dados una funcion de la Tarea 1 y un Nivel completo, cuenta cuantas celdas hay que cumplen con la funcion dada
-- IMPLEMENTACION: Miriam
-- Cuenta cuántas celdas del nivel cumplen una condición.
-- Usamos foldl porque vamos acumulando una cantidad numérica,
-- sin necesitar conservar la estructura ni hacer cortocircuito.
contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Int
contarCeldasQueCumplen condicion nivel =
    foldl sumarFila 0 nivel -- empiezo contando en cero y voy sumando las celdas válidas
  where
    sumarFila :: Int -> String -> Int
    sumarFila acumulador fila =
        acumulador + contarFila fila

    contarFila :: String -> Int
    contarFila fila =
        foldl contarCelda 0 fila

    contarCelda :: Int -> Celda -> Int
    contarCelda acumulador celda
      | condicion celda = acumulador + 1
      | otherwise = acumulador

-- Bonus: Propiedades con QuickCheck
-- IMPLEMENTACION: Jesús

-- prop_mapPropio_equivale_map --> comprueba que la funcion mapPropio es equivalente a map
prop_mapPropio_equivale_map :: (Num a, Eq a) => [a] -> Bool
prop_mapPropio_equivale_map xs = mapPropio (^2) xs == map (^2) xs
-- ghci> quickCheck prop_mapPropio_equivale_map
-- +++ OK, passed 100 tests.

-- prop_filterPropio_equivale_filter --> comprueba que la funcion filterPropio es equivalente a filter
prop_filterPropio_equivale_filter :: (Integral a, Eq a) => [a] -> Bool
prop_filterPropio_equivale_filter xs = filterPropio odd xs == filter odd xs
-- ghci> quickCheck prop_filterPropio_equivale_filter
-- +++ OK, passed 100 tests.

-- prop_foldrPropio_equivale_foldr --> comprueba que la funcion foldrPropio es equivalente a foldr
prop_foldrPropio_equivale_foldr :: [Int] -> Bool
prop_foldrPropio_equivale_foldr xs = foldrPropio (-) 0 xs == foldr (-) 0 xs
-- ghci> quickCheck prop_foldrPropio_equivale_foldr
-- +++ OK, passed 100 tests.

-- prop_foldlPropio_equivale_foldl --> comprueba que la funcion foldlPropio es equivalente a foldl
prop_foldlPropio_equivale_foldl :: [Int] -> Bool
prop_foldlPropio_equivale_foldl xs = foldlPropio (*) 0 xs == foldl (*) 0 xs
-- ghci> quickCheck prop_foldlPropio_equivale_foldl
-- +++ OK, passed 100 tests.

-- prop_zipPropio_equivale_zip --> comprueba que la funcion zipPropio es equivalente a zip
prop_zipPropio_equivale_zip :: (Eq a, Eq b) => [a] -> [b] -> Bool
prop_zipPropio_equivale_zip xs ys = zipPropio xs ys == zip xs ys
-- ghci> quickCheck prop_zipPropio_equivale_zip
-- +++ OK, passed 100 tests.

-- prop_simularCaidaLibre_longitud --> comprueba que la longitud del resultado de la funcion simularCaidaLibre es siempre igual al numero de pasos + 1
prop_simularCaidaLibre_longitud :: Int -> Double -> (Double, Double) -> Property
prop_simularCaidaLibre_longitud p a (x, v) = p >= 0 ==> length (simularCaidaLibre p a (x,v)) == p + 1
-- ghci> quickCheck prop_simularCaidaLibre_longitud
-- +++ OK, passed 100 tests; 102 discarded.

-- prop_hayColisionConAlguna_lista_vacia --> Comprueba que para una lista vacia de CajaColision (obstaculos), el resultado de 'hayColisionConAlguna' es siempre False
prop_hayColisionConAlguna_lista_vacia :: CajaColision -> [CajaColision] -> Bool
prop_hayColisionConAlguna_lista_vacia cajaInicial xs = hayColisionConAlguna cajaInicial [] == False
-- ghci> quickCheck prop_hayColisionConAlguna_lista_vacia
-- +++ OK, passed 100 tests.