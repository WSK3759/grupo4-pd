import Test.QuickCheck

--------------------------------------------------
-- TIPOS SINÓNIMOS
--------------------------------------------------

type Vector2D = (Double, Double)

type CajaColision = (Double, Double, Double, Double) -- Posición (x,y), ancho y alto

type Celda = Char

type Grid = [String] -- Un Grid es una lista de filas, donde cada fila es un String


--------------------------------------------------
-- TAREA 1 (MAIN)
--------------------------------------------------

--------------------------------------------------
-- 1. VECTORES 2D
--------------------------------------------------

-- Suma dos vectores 2D componente a componente
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores v1 v2 =
    (fst v1 + fst v2, snd v1 + snd v2)


-- Multiplica un vector 2D por un factor escalar
escalarVector :: Double -> Vector2D -> Vector2D
escalarVector x v =
    (x * fst v, x * snd v)


-- Calcula la distancia euclídea entre dos puntos 2D
distancia :: Vector2D -> Vector2D -> Double
distancia v1 v2 =
    sqrt ((fst v1 - fst v2)^2 + (snd v1 - snd v2)^2)


--------------------------------------------------
-- 2. CAJAS DE COLISIÓN
--------------------------------------------------

-- Indica si dos cajas de colisión se solapan en ambos ejes
solapan :: CajaColision -> CajaColision -> Bool
solapan (x1,y1,ancho1,alto1) (x2,y2,ancho2,alto2) =
    x1 + ancho1 > x2 &&
    x2 + ancho2 > x1 &&
    y1 + alto1 > y2 &&
    y2 + alto2 > y1


--------------------------------------------------
-- 3. LADO DE COLISIÓN
--------------------------------------------------

-- Devuelve el lado por el que se produce la colisión según el menor solape
ladoColision :: CajaColision -> CajaColision -> String
ladoColision (x1, y1, a1, h1) (x2, y2, a2, h2)
    | arriba <= abajo
      && arriba <= izquierda
      && arriba <= derecha = "Arriba"

    | abajo <= arriba
      && abajo <= izquierda
      && abajo <= derecha = "Abajo"

    | izquierda <= arriba
      && izquierda <= abajo
      && izquierda <= derecha = "Izquierda"

    | otherwise = "Derecha"

    where
        arriba    = (y2 + h2) - y1
        abajo     = (y1 + h1) - y2
        izquierda = (x1 + a1) - x2
        derecha   = (x2 + a2) - x1


--------------------------------------------------
-- 4. UTILIDADES DE LISTAS Y CADENAS
--------------------------------------------------

-- Divide una cadena en trozos utilizando un carácter como separador
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
    | c == x    = "" : lista
    | otherwise = (x : head lista) : tail lista
    where
        lista = splitOn c xs


-- Elimina espacios, tabuladores y saltos de línea al principio y al final
trim :: String -> String
trim xs =
    reverse (dropWhile esEspacio (reverse (dropWhile esEspacio xs)))
    where
        esEspacio :: Char -> Bool
        esEspacio c = c `elem` [' ', '\t', '\n']


-- Cuenta cuántos elementos de una lista cumplen una condición dada
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple p xs =
    length [x | x <- xs, p x]


-- Convierte los dos primeros elementos de una lista en un vector 2D.
list2Vector2 :: [Double] -> Vector2D
list2Vector2 [] =
    error "head : Lista vacia no posible convertir en Vector2"
list2Vector2 [x] =
    error "head : Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) =
    (x,y)


--------------------------------------------------
-- 5. PARSEO DEL NIVEL
--------------------------------------------------

-- Convierte una lista de filas de texto en un Grid
parsearNivel :: [String] -> Grid
parsearNivel s = s


-- Indica si una celda representa una plataforma sólida
esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _   = False


-- Indica si una celda representa la meta del nivel
esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _   = False


-- Indica si una celda está vacía
esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _   = False


-- Indica si una celda representa el inicio de un enemigo
esEnemigo :: Celda -> Bool
esEnemigo celda =
    not (esSolido celda || esMeta celda || esVacio celda)


-- Devuelve las posiciones (fila,columna) en las que aparece la meta
posicionesMeta :: Grid -> [(Int, Int)]
posicionesMeta nivel =
    [ (f,c)
    | (f,fila) <- zip [0..] nivel
    , (c,celda) <- zip [0..] fila
    , esMeta celda
    ]


-- Devuelve la posición y el identificador de cada enemigo del nivel
posicionesEnemigos :: Grid -> [(Int, Int, Char)]
posicionesEnemigos nivel =
    [ (indiceLista, indiceString, caracter)
    | (indiceLista,stringLista) <- zip [0..] nivel
    , (indiceString,caracter) <- zip [0..] stringLista
    , esEnemigo caracter
    ]


-- Agrupa las rachas de celdas sólidas indicando posición inicial y longitud
agruparRachas :: String -> [(Int, Int)]
agruparRachas fila =
    aux 0 fila
    where
        aux :: Int -> String -> [(Int, Int)]
        aux _ "" = []

        aux indice xs
            | not (esSolido (head xs)) =
                let (_,resto) = span (not . esSolido) xs
                    saltados = length xs - length resto
                in aux (indice + saltados) resto

            | otherwise =
                let (solidos,resto) = span esSolido xs
                    longitud = length solidos
                in (indice,longitud)
                   : aux (indice + longitud) resto


--------------------------------------------------
-- 6. BONUS: PROPIEDADES CON QUICKCHECK
--------------------------------------------------

-- Comprueba que la suma de vectores es conmutativa
prop_suma_conmutativa :: Vector2D -> Vector2D -> Bool
prop_suma_conmutativa a b =
    sumaVectores a b == sumaVectores b a

-- +++ OK, passed 100 tests.


-- Comprueba la propiedad asociativa de la suma de vectores
prop_suma_asociativa :: Vector2D -> Vector2D -> Vector2D -> Bool
prop_suma_asociativa a b c =
    sumaVectores (sumaVectores a b) c
    ==
    sumaVectores a (sumaVectores b c)

-- Failed! Falsified (after 9 tests and 12 shrinks):
-- (0.6,0.0)
-- (0.3,0.0)
-- (4.0,0.0)
-- Falla por pequeños errores acumulados al redondear.


-- Comprueba que multiplicar un vector por 1 no lo modifica
prop_escalar_neutro :: Vector2D -> Bool
prop_escalar_neutro v =
    escalarVector 1 v == v

-- +++ OK, passed 100 tests.


-- Comprueba que la distancia entre dos puntos nunca es negativa
prop_distancia_no_negativa :: Vector2D -> Vector2D -> Bool
prop_distancia_no_negativa a b =
    distancia a b >= 0

-- +++ OK, passed 100 tests.


-- Comprueba que la distancia entre dos puntos es simétrica
prop_distancia_simetrica :: Vector2D -> Vector2D -> Bool
prop_distancia_simetrica a b =
    distancia a b == distancia b a

-- +++ OK, passed 100 tests.


-- Comprueba que el solape entre dos cajas es simétrico
prop_solapan_simetrica :: CajaColision -> CajaColision -> Bool
prop_solapan_simetrica a b =
    solapan a b == solapan b a

-- +++ OK, passed 100 tests.


-- Comprueba que aplicar trim dos veces equivale a aplicarlo una vez
prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s =
    trim (trim s) == trim s

-- +++ OK, passed 100 tests.


-- Comprueba que sin separador splitOn devuelve únicamente la cadena original
prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador c s =
    notElem c s ==> splitOn c s == [s]

-- +++ OK, passed 100 tests; 14 discarded.


-- Comprueba que contarSiCumple nunca supera la longitud de la lista
prop_contarSiCumple_acotado :: [Int] -> Bool
prop_contarSiCumple_acotado xs =
    contarSiCumple even xs <= length xs


--------------------------------------------------
-- TAREA 2
--------------------------------------------------

-- **********************************
-- BLOQUE 1
-- **********************************

--------------------------------------------------
-- 1. Reimplementación recursiva de las consultas del nivel
--------------------------------------------------

-- Devuelve recursivamente las posiciones (fila,columna) en las que aparece la meta
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


-- Devuelve recursivamente las posiciones y el identificador de los enemigos.
posicionesEnemigosRec :: Grid -> [(Int, Int, String)]
posicionesEnemigosRec nivel = filas 0 nivel
  where
    filas :: Int -> Grid -> [(Int, Int, String)]
    filas _ [] = []
    filas f (fila:resto) =
        columnas f 0 fila ++ filas (f+1) resto

    columnas :: Int -> Int -> String -> [(Int, Int, String)]
    columnas _ _ [] = []
    columnas f c (x:xs)
      | esEnemigo x = (f,c,[x]) : columnas f (c+1) xs -- String = [Char]
      | otherwise = columnas f (c+1) xs

--------------------------------------------------
-- 2 Generalización por recursión de agruparRachas
--------------------------------------------------

-- Agrupa las rachas de caracteres iguales indicando inicio, longitud y carácter.
agruparPorCaracter :: String -> [(Int, Int, Char)]
agruparPorCaracter fila = agrupar 0 fila
  where
    agrupar :: Int -> String -> [(Int, Int, Char)]
    agrupar _ [] = [] -- CB
    agrupar indice (caracter:restoFila) = -- indice: posición donde empieza la racha; caracter: primer caracter; restoFila: resto de la cadena
        (indice, longitud, caracter) : agrupar (indice + longitud) resto
      where
        (longitud, resto) = medirRacha 1 caracter restoFila

    medirRacha :: Int -> Char -> String -> (Int, String)
    medirRacha cantidad _ [] = (cantidad, [])
    medirRacha cantidad caracter (siguiente:resto)
      | caracter == siguiente =
          medirRacha (cantidad + 1) caracter resto
      | otherwise =
          (cantidad, siguiente:resto)

--------------------------------------------------
-- 3. Recursión numérica: simulación de movimiento
--------------------------------------------------

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

--------------------------------------------------
-- 4. Recursión sobre dos listas a la vez
--------------------------------------------------

-- Empareja los elementos de dos listas hasta que una de las dos se acaba.
zipPropio :: [a] -> [b] -> [(a,b)]
zipPropio [] _ = [] -- CB: si la primera lista se acaba, terminamos
zipPropio _ [] = [] -- CB: si la segunda lista se acaba, terminamo
zipPropio (x:xs) (y:ys) =
    (x,y) : zipPropio xs ys

--------------------------------------------------
-- 5. Recursión con descarte condicional: preparación de líneas
--------------------------------------------------

-- Comprueba si una línea está vacía o es un comentario.
esComentarioOVacia :: String -> Bool
esComentarioOVacia [] = True
esComentarioOVacia (';':_) = True -- si empieza por ; devuelve True
esComentarioOVacia _ = False

-- Limpia las líneas y elimina las vacías o los comentarios.
prepararLineas :: [String] -> [String]
prepararLineas [] = []   -- CB: no quedan líneas
prepararLineas (linea:resto)
    | esComentarioOVacia lineaLimpia = prepararLineas resto -- si es comentario o vacía, la ignoramos y seguimos con el resto
    | otherwise = lineaLimpia : prepararLineas resto -- si no es comentario ni vacía, la guardamos
  where
    lineaLimpia = trim linea -- limpia la línea de espacios

-- **********************************
-- BLOQUE 2
-- **********************************

--------------------------------------------------
-- 5. mapPropio y filterPropio
--------------------------------------------------

-- Aplica una función a todos los elementos de una lista.
mapPropio :: (a -> b) -> [a] -> [b]
mapPropio _ [] = []   -- CB: no quedan elementos
mapPropio funcion (x:xs) =
    funcion x : mapPropio funcion xs

-- Se queda solo con los elementos que cumplen una condición.
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ [] = []   -- CB: no quedan elementos
filterPropio condicion (x:xs)
    | condicion x = x : filterPropio condicion xs
    | otherwise = filterPropio condicion xs -- si no cumple la condición no guardamos el elemento x

--------------------------------------------------
-- 6. foldrPropio y foldlPropio
--------------------------------------------------

-- Combina los elementos de una lista de derecha a izquierda.
foldrPropio :: (a -> b -> b) -> b -> [a] -> b
foldrPropio _ valorInicial [] = valorInicial   -- CB: no quedan elementos
foldrPropio funcion valorInicial (x:xs) =
    funcion x (foldrPropio funcion valorInicial xs)

-- Combina los elementos de una lista de izquierda a derecha usando un acumulador.
foldlPropio :: (b -> a -> b) -> b -> [a] -> b
foldlPropio _ acumulador [] = acumulador   -- CB: no quedan elementos
foldlPropio funcion acumulador (x:xs) = -- Va guardando en un acumulador el resultado y sigue con el resto de la lista.
    foldlPropio funcion
    (funcion acumulador x) -- (+) 0 1 -> 0+1 (nuevo acumulador)
    xs


--------------------------------------------------
-- 7. Aplicación justificada de foldr y foldl
--------------------------------------------------

-- Comprueba si una caja colisiona con alguna de una lista de cajas.
-- Usamos foldr porque (||) puede dejar de evaluar cuando encuentra True,
-- por lo que no necesita recorrer necesariamente toda la lista.
hayColisionConAlguna :: CajaColision -> [CajaColision] -> Bool
hayColisionConAlguna caja obstaculos =
    foldr comprobar False obstaculos -- si no hay ninguna caja el resultado es False
  where
    comprobar :: CajaColision -> Bool -> Bool
    comprobar obstaculo resultado =
        solapan caja obstaculo || resultado  -- busca si hay al menos un True

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


--------------------------------------------------
-- BONUS: Propiedades con QuickCheck
--------------------------------------------------

-- Comprueba que mapPropio da el mismo resultado que map.
prop_mapPropio_equivale_map :: [Int] -> Bool
prop_mapPropio_equivale_map xs =
    mapPropio (*2) xs == map (*2) xs

-- Comprueba que filterPropio da el mismo resultado que filter.
prop_filterPropio_equivale_filter :: [Int] -> Bool
prop_filterPropio_equivale_filter xs =
    filterPropio even xs == filter even xs

-- Comprueba que foldrPropio da el mismo resultado que foldr.
prop_foldrPropio_equivale_foldr :: [Int] -> Bool
prop_foldrPropio_equivale_foldr xs =
    foldrPropio (+) 0 xs == foldr (+) 0 xs

-- Comprueba que foldlPropio da el mismo resultado que foldl.
prop_foldlPropio_equivale_foldl :: [Int] -> Bool
prop_foldlPropio_equivale_foldl xs =
    foldlPropio (+) 0 xs == foldl (+) 0 xs

-- Comprueba que zipPropio da el mismo resultado que zip.
prop_zipPropio_equivale_zip :: [Int] -> String -> Bool
prop_zipPropio_equivale_zip xs ys =
    zipPropio xs ys == zip xs ys

-- Comprueba que con n pasos se obtienen n+1 estados.
prop_simularCaidaLibre_longitud :: Int -> Double -> Double -> Double -> Property
prop_simularCaidaLibre_longitud n gravedad posicion velocidad =
    n >= 0 ==>
        length (simularCaidaLibre n gravedad (posicion, velocidad)) == n + 1

-- Comprueba que no hay colisión si la lista de obstáculos está vacía.
prop_hayColisionConAlguna_lista_vacia :: CajaColision -> Bool
prop_hayColisionConAlguna_lista_vacia caja =
    hayColisionConAlguna caja [] == False
