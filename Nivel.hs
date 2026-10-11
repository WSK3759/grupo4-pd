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


--------------------------------------------------------------------------------------------------------------
-- TAREA 2
--------------------------------------------------------------------------------------------------------------

-- 1.1 posicionesMetaRec

posicionesMetaRec :: Grid -> [(Int, Int)] -- Devuelve la lista de posiciones (fila, columna) de la meta mediante recursión explícita.
posicionesMetaRec nivel = auxFilas 0 nivel
  where
    auxFilas :: Int -> Grid -> [(Int, Int)]
    auxFilas indiceFila [] = []
    auxFilas indiceFila (fila:filas) = 
        auxColumnas indiceFila 0 fila ++ auxFilas (indiceFila + 1) filas

    auxColumnas :: Int -> Int -> String -> [(Int, Int)]
    auxColumnas _ _ [] = []
    auxColumnas f c (caracter:xs) 
        | esMeta caracter = (f, c) : auxColumnas f (c + 1) xs
        | otherwise = auxColumnas f (c + 1) xs


-- 1.2 posicionesEnemigosRec

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


-- 2.1 agruparPorCaracter

agruparPorCaracter :: String -> [(Int, Int, Char)] -- Agrupa caracteres consecutivos iguales en tríos de posición, longitud y carácter.
agruparPorCaracter fila = auxColumnas 0 fila
  where
    auxColumnas :: Int -> String -> [(Int, Int, Char)]
    auxColumnas _ [] = []
    auxColumnas indiceColumna (c:xs) =
        let (iguales, resto) = span (== c) (c:xs)
            longitud = length iguales
        in (indiceColumna, longitud, c) : auxColumnas (indiceColumna + longitud) resto


-- 3.1 simularCaidaLibre 
simularCaidaLibre :: Int -> Double -> (Double, Double) -> [(Double, Double)] -- Simula la caída libre paso a paso devolviendo la lista de estados (posición, velocidad).
simularCaidaLibre 0 _ (pos, vel) = [(pos, vel)]
simularCaidaLibre n gravedad (pos, vel) = 
    (pos, vel) : simularCaidaLibre (n - 1) gravedad (pos + vel, vel + gravedad)


-- 4.1 zipPropio
zipPropio :: [a] -> [b] -> [(a, b)]-- Empareja los elementos de dos listas uno a uno deteniéndose cuando se acaba alguna.
zipPropio [] _ = []
zipPropio _ [] = []
zipPropio (x:xs) (y:ys) = (x, y) : zipPropio xs ys

-- 5.1 esComentarioOVacia y prepararLineas
esComentarioOVacia :: String -> Bool
esComentarioOVacia "" = True
esComentarioOVacia s = head s == ';' 

prepararLineas :: [String] -> [String]
prepararLineas [] =[]
prepararLineas (x:xs) 
    | esComentarioOVacia x = prepararLineas xs
    | otherwise = x: prepararLineas xs


--------------------------------------------------------------------------------------------------------------
-- BLOQUE 2
--------------------------------------------------------------------------------------------------------------

-- 5.1 mapPropio
mapPropio:: (Int->Int) -> [Int]->[Int]
mapPropio _ [] = []
mapPropio operacion (x:xs) = operacion x : mapPropio operacion xs

-- 5.2 filterPropio
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ [] = []
filterPropio cond (x:xs) 
    | cond x = x : filterPropio cond xs
    | otherwise = filterPropio cond xs

-- 6.1 foldrPropio
foldrPropio:: (a -> b -> b) -> b -> [a] -> b   -- Combina los elementos de una lista de derecha a izquierda usando una función y un valor inicial.
foldrPropio _ ac [] = ac
foldrPropio funcion ac (x:xs) = funcion x (foldrPropio funcion ac xs )

-- 6.2 foldlPropio
foldlPropio :: (b -> a -> b) -> b -> [a] -> b -- Combina los elementos de una lista de izquierda a derecha arrastrando un acumulador. 
foldlPropio _ ac [] = ac
foldlPropio f ac (x:xs) = foldlPropio f (f ac x) xs


-- 7.1 hayColisionConAlguna
-- Se elige foldr (y no foldl) porque permite cortocircuito (lazy evaluation) con el operador ||,
-- deteniendo la búsqueda de inmediato al encontrar la primera colisión sin necesidad de procesar toda la lista.
hayColisionConAlguna :: CajaColision -> [CajaColision] -> Bool
hayColisionConAlguna caja = foldr (\obstaculo acumulador-> solapan caja obstaculo || acumulador) False

-- 7.2 contarCeldasQueCumplen  

-- Se elige foldl porque se trata de una acumulación numérica lineal que procesa 
-- todos los elementos sin necesidad de cortocircuito ni de reconstruir estructuras
contarCeldasQueCumplen :: (Char -> Bool) -> Grid -> Int
contarCeldasQueCumplen cond nivel = foldl (\ac fila -> ac + contarFila fila) 0 nivel
  where
    contarFila :: String -> Int
    contarFila fila = foldl (\ac c -> if cond c then ac + 1 else ac) 0 fila

-- Bonus: Propiedades con QuickCheck

-- 1. Propiedad de equivalencia para mapPropio usando (*2)
prop_mapPropio_equivale_map :: [Int] -> Bool
prop_mapPropio_equivale_map xs = mapPropio (*2) xs == map (*2) xs

-- 2. Propiedad de equivalencia para filterPropio usando even
prop_filterPropio_equivale_filter :: [Int] -> Bool
prop_filterPropio_equivale_filter xs = filterPropio even xs == filter even xs

-- 3. Propiedad de equivalencia para foldrPropio usando (+)
prop_foldrPropio_equivale_foldr :: Int -> [Int] -> Bool
prop_foldrPropio_equivale_foldr z xs = foldrPropio (+) z xs == foldr (+) z xs

-- 4. Propiedad de equivalencia para foldlPropio usando (+)
prop_foldlPropio_equivale_foldl :: Int -> [Int] -> Bool
prop_foldlPropio_equivale_foldl z xs = foldlPropio (+) z xs == foldl (+) z xs

-- 5. Propiedad de equivalencia para zipPropio
prop_zipPropio_equivale_zip :: [Int] -> [Int] -> Bool
prop_zipPropio_equivale_zip xs ys = zipPropio xs ys == zip xs ys

-- 6. Propiedad condicional para simularCaidaLibre (longitud n + 1)
prop_simularCaidaLibre_longitud :: Int -> Double -> (Double, Double) -> Property
prop_simularCaidaLibre_longitud n gravedad estado =
    n >= 0 ==> length (simularCaidaLibre n gravedad estado) == n + 1

-- 7. Propiedad para hayColisionConAlguna con lista vacía
prop_hayColisionConAlguna_lista_vacia :: CajaColision -> Bool
prop_hayColisionConAlguna_lista_vacia caja = not (hayColisionConAlguna caja [])
