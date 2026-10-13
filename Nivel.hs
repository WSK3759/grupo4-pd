import Test.QuickCheck

-- DECLARACIONES DE TIPO (tipos sinónimos):
-- 1 VECTORES 2D
type Vector2D = (Double, Double) -- A. Definición del tipo sinónimo para un punto/vector 2D (1 Vectores 2D)
-- 2 CAJAS DE COLISION
type CajaColision = (Double, Double, Double, Double) -- B. Definición del tipo sinónimo para una delimitadora (2 Cajas de colisión)
-- 5 PARSEO DEL NIVEL 
type Celda = Char -- C. Definición del tipo sinonimo 'Celda' para representar una celda de un nivel como un carácter
type Nivel = [[Celda]] -- D. Definición del tipo sinonimo 'Nivel' para representar un nivel completo como una serie de Celdas 


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


-- Ejercicio 3 Lado de Colisión

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

parsearNivel :: [String] -> Nivel
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

esEnemigo :: Celda -> Bool
esEnemigo celda = not (esSolido celda || esMeta celda || esVacio celda)

posicionesMeta :: Nivel -> [(Int, Int)]
posicionesMeta nivel = [ (f, c)
  | (f, fila) <- zip [0..] nivel
  , (c, celda) <- zip [0..] fila
  , esMeta celda
  ]

posicionesEnemigos :: Nivel -> [(Int, Int, Char)]
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



--ENTREGA 2
--EJERCICIO 1
--Devolver las posiciones en las que aparece la meta
posicionesMetaRec :: Nivel -> [(Int, Int)]
posicionesMetaRec n = filas 0 n 
    where
        filas _ [] = []
        filas f (fila:resto) = columnas f 0 fila ++ filas (f+1) resto 
        columnas _ _ [] = []
        columnas f c (x:xs)
            | esMeta x  = (f,c) : columnas f (c+1) xs
            | otherwise = columnas f (c+1) xs 

--Devolver las posiciones de los enemigos
posicionesEnemigosRec :: Nivel -> [(Int, Int, Char)]
posicionesEnemigosRec n = filas 0 n
    where
        filas _ [] = []
        filas f (fila:resto) = columnas f 0 fila ++ filas (f+1) resto 
        columnas _ _ [] = []
        columnas f c (x:xs)
            | esEnemigo x = (f,c,x) : columnas f (c+1) xs
            | otherwise   = columnas f (c+1) xs

--EJERCICIO 2
--Agrupar varios caracteres seguidos de una cadena que sean iguales
agruparPorCaracter :: String -> [(Int, Int, Char)]
agruparPorCaracter fila = aux 0 fila 
    where
        aux :: Int -> String -> [(Int, Int, Char)]
        aux _ "" = []
        aux indice xs = (indice, final, head caracter) : aux (indice+final) resto
            where
                (caracter, resto ) = span (== head xs) xs 
                final = length caracter 

--EJERCICIO 3
--Muesta todos los estado por los que ha pasado un objeto en movimiento 
simularCaidaLibre :: Int -> Double -> Vector2D -> [Vector2D]
simularCaidaLibre 0 _ p = [p]
simularCaidaLibre pasos ace  (x,y) 
    | pasos > 0 = (x,y) : simularCaidaLibre (pasos-1) ace (x+y, ace+y)
    | otherwise = (x,y): simularCaidaLibre 0 ace (x,y)

--EJERCICIO 4
--Emparejar dos listas
zipPropio :: [a] -> [a] -> [(a, a)]
zipPropio [] _ = []
zipPropio _ [] = []
zipPropio (x:xs) (y:ys) 
    | (length xs) > 0 && (length ys) > 0 = (x,y) : zipPropio xs ys
    | otherwise                          = [(x,y)]

--EJERCICIO 5
--Determinar si una línea es un comentario o está vacía
esComentarioOVacia :: String -> Bool
esComentarioOVacia linea 
    | trim linea == ""           = True
    | head (trim linea)  == ';'  = True
    | otherwise                  = False

--Aplicar la función anterior a un conjunto de líneas
prepararLineas :: [String] -> [String]
prepararLineas [] = []
prepararLineas (x:xs)
    | esComentarioOVacia x == True = prepararLineas xs
    | otherwise                    =  trim x : prepararLineas xs

--SUBTAREA 5
-- Aplicar una función a cada elemento de una lista sin alterar el orden
mapPropio:: (a -> b) -> [a] -> [b]
mapPropio _ [] = []
mapPropio f (x:xs) = f x : mapPropio f xs

-- Aplicar un filtro a una lista
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ [] = []
filterPropio cond (x:xs) 
    | cond x    = x : filterPropio cond xs
    | otherwise = filterPropio cond xs

--SUBTAREA 6
-- Combinar los elementos de una lista de derecha a izquierda usando una función binaria y un valor inicial
foldrPropio:: (a -> b -> b) -> b -> [a] -> b   
foldrPropio _ ac [] = ac
foldrPropio f ac (x:xs) = f x (foldrPropio f ac xs )

-- Combinar los elementos de una lista de izquierda a derecha con un acumulador
foldlPropio :: (b -> a -> b) -> b -> [a] -> b 
foldlPropio _ ac [] = ac
foldlPropio f ac (x:xs) = foldlPropio f (f ac x) xs

--SUBTAREA 7
-- Indicar si una caja colisiona con otras
-- Para aprovechar el cortocircuito de (||) ante listas largas de obstáculos, comprobamos que el operador más eficiente
-- es foldr, ya que con éste, si encontramos un caso que cumpla la condición la lista no se recorre más por lo que es mas eficiente
hayColisionConAlguna :: CajaColision -> [CajaColision] -> Bool
hayColisionConAlguna cajaInicial obstaculos = foldr auxCombinadora False obstaculos
    where
        auxCombinadora caja resto = solapan cajaInicial caja || resto

-- Contar cuantas celdas cumplen una condición
-- En este caso no estamos buscando cumplir una condición, simplemente queremos un recuento sobre una lista, por tanto 
-- lo adecuado sería utilizar foldl
contarCeldasQueCumplen :: (Celda -> Bool) -> Nivel -> Int
contarCeldasQueCumplen f = foldl contarCeldasFila 0
    where
    contarCeldasFila :: Int -> [Celda] -> Int
    contarCeldasFila ac fila = ac + contarFila fila

    contarFila :: [Celda] -> Int
    contarFila = foldl contarCelda 0

    contarCelda :: Int -> Celda -> Int
    contarCelda ac celda
        | f celda   = ac + 1
        | otherwise = ac

--EJERCICIO EXTRA
-- Comprueba que mapPropio equivale a map
prop_mapPropio_equivale_map :: [Int] -> Bool
prop_mapPropio_equivale_map xs =
    mapPropio (+1) xs == map (+1) xs
-- +++ OK, passed 100 tests.

-- Comprueba que filterPropio produce el mismo resultado que filter
prop_filterPropio_equivale_filter :: [Int] -> Bool
prop_filterPropio_equivale_filter xs =
    filterPropio even xs == filter even xs
-- +++ OK, passed 100 tests.

-- Comprueba que foldrPropio produce el mismo resultado que foldr
prop_foldrPropio_equivale_foldr :: [Int] -> Bool
prop_foldrPropio_equivale_foldr xs =
    foldrPropio (+) 0 xs == foldr (+) 0 xs
-- +++ OK, passed 100 tests.

-- Comprueba que foldlPropio produce el mismo resultado que foldl
prop_foldlPropio_equivale_foldl :: [Int] -> Bool
prop_foldlPropio_equivale_foldl xs =
    foldlPropio (+) 0 xs == foldl (+) 0 xs
-- +++ OK, passed 100 tests.

-- Comprueba que zipPropio produce el mismo resultado que zip
prop_zipPropio_equivale_zip :: [Int] -> [Int] -> Bool
prop_zipPropio_equivale_zip xs ys =
    zipPropio xs ys == zip xs ys
-- +++ OK, passed 100 tests.

-- Comprueba que una simulación de n pasos genera n + 1 estados
prop_simularCaidaLibre_longitud :: Int -> Property
prop_simularCaidaLibre_longitud n =
    n >= 0 ==> length (simularCaidaLibre n (-1) (0,0)) == n + 1
-- +++ OK, passed 100 tests; 90 discarded.

-- Comprueba que una lista vacía de obstáculos nunca produce colisión
prop_hayColisionConAlguna_lista_vacia :: CajaColision -> Bool
prop_hayColisionConAlguna_lista_vacia caja =
    hayColisionConAlguna caja [] == False
-- +++ OK, passed 100 tests.
