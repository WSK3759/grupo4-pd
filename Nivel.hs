module Nivel
    ( ErrorNivel(..)
    , NivelData(..)
    , buscarClave
    , splitOn
    , parsearFichero
    , validarNivel
    , parsearNivelCompleto
    , tamCelda
    , posicionCelda
    , gridAPlataformas
    , metasDesdeGrid
    , enemigosDesdeGrid
    ) where

import Tipos
import Objeto
import Test.QuickCheck
import Data.List (stripPrefix)
import Text.Read (readMaybe)

distancia :: Vector2D -> Vector2D -> Double
distancia v1 v2 = sqrt((fst v1 - fst v2)^2 + (snd v1 - snd v2)^2)



-- 3 Lado de Colisión

ladoColision :: BoundingBox -> BoundingBox -> String
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
-- Convierte las filas de caracteres en una cuadrícula de celdas.
parsearNivel :: [String] -> Grid
parsearNivel = parsearGrid

-- Convierte un carácter en su celda correspondiente.

-- Devuelve las posiciones de la meta.
posicionesMeta :: Grid -> [(Int, Int)]
posicionesMeta nivel =
    [ (f, c)
    | (f, fila) <- zip [0..] nivel
    , (c, celda) <- zip [0..] fila
    , esMeta celda
    ]

-- Devuelve las posiciones y los identificadores de los enemigos.
posicionesEnemigos :: Grid -> [(Int, Int, Char)]
posicionesEnemigos nivel =
    [ (f, c, identificador)
    | (f, fila) <- zip [0..] nivel
    , (c, celda) <- zip [0..] fila
    , InicioEnemigo (identificador:_) <- [celda]
    ]

-- Agrupa las rachas de sólidos consecutivos de una fila de caracteres.
-- Comprueba si un carácter representa un sólido.
esSolidoChar :: Char -> Bool
esSolidoChar '#' = True
esSolidoChar _   = False

agruparRachas :: String -> [(Int, Int)]
agruparRachas fila = aux 0 fila
  where
    aux :: Int -> String -> [(Int, Int)]
    aux _ [] = []
    aux indice xs
      | not (esSolidoChar (head xs)) =
          let (_, resto) = span (not . esSolidoChar) xs
              saltados = length xs - length resto
          in aux (indice + saltados) resto
      | otherwise =
          let (solidos, resto) = span esSolidoChar xs
              longitud = length solidos
          in (indice, longitud) : aux (indice + longitud) resto

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
prop_solapan_simetrica :: BoundingBox -> BoundingBox -> Bool
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

-- Busca recursivamente las posiciones de la meta.
posicionesMetaRec :: Grid -> [(Int, Int)]
posicionesMetaRec nivel = filas 0 nivel
  where
    filas :: Int -> Grid -> [(Int, Int)]
    filas _ [] = []
    filas f (fila:resto) =
        columnas f 0 fila ++ filas (f + 1) resto

    columnas :: Int -> Int -> [Celda] -> [(Int, Int)]
    columnas _ _ [] = []
    columnas f c (x:xs)
      | esMeta x  = (f, c) : columnas f (c + 1) xs
      | otherwise = columnas f (c + 1) xs

-- Busca recursivamente las posiciones e identificadores de los enemigos.
posicionesEnemigosRec :: Grid -> [(Int, Int, Char)]
posicionesEnemigosRec nivel = filas 0 nivel
  where
    filas :: Int -> Grid -> [(Int, Int, Char)]
    filas _ [] = []
    filas f (fila:resto) =
        columnas f 0 fila ++ filas (f + 1) resto

    columnas :: Int -> Int -> [Celda] -> [(Int, Int, Char)]
    columnas _ _ [] = []
    columnas f c (celda:xs) =
        case celda of
          InicioEnemigo (identificador:_) ->
              (f, c, identificador) : columnas f (c + 1) xs
          _ -> columnas f (c + 1) xs

-- 2 Generalización por recursión de agruparRachas

-- Función: agruparPorCaracter -> Recorre una fila y 
-- agrupa en tríos (columna de inicio, longitud, carácter) las rachas de caracteres consecutivos iguales.
-- IMPLEMENTACION: Jesús

agruparPorCaracter :: [Celda] -> [(Int, Int, Celda)]
agruparPorCaracter [] = [] -- caso base: si la fila esta vacia, devuelve una lista vacia
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
hayColisionConAlguna :: BoundingBox -> [BoundingBox] -> Bool
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
-- Cuenta las celdas de un nivel que cumplen una condición.
contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Int
contarCeldasQueCumplen condicion nivel =
    foldl sumarFila 0 nivel
  where
    sumarFila :: Int -> [Celda] -> Int
    sumarFila acumulador fila =
        acumulador + foldl contarCelda 0 fila

    contarCelda :: Int -> Celda -> Int
    contarCelda acumulador celda
      | condicion celda = acumulador + 1
      | otherwise       = acumulador

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
prop_hayColisionConAlguna_lista_vacia :: BoundingBox -> [BoundingBox] -> Bool
prop_hayColisionConAlguna_lista_vacia cajaInicial xs = hayColisionConAlguna cajaInicial [] == False
-- ghci> quickCheck prop_hayColisionConAlguna_lista_vacia
-- +++ OK, passed 100 tests.

--ENTREGA 3
-- Bloque 1
--Subtarea 1
-- Representa los tipos de celdas que puede contener un nivel.



{-
-- Convierte un carácter en su celda correspondiente.
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = Enemigo [c]

-- Convierte una lista de filas de caracteres en una cuadrícula de celdas.
parsearGrid :: [String] -> Grid
parsearGrid = map (map parsearCelda)

celdaAChar :: Celda -> Char
celdaAChar Solido = '#'
celdaAChar Vacio = '.'
celdaAChar META = 'M'
celdaAChar (Enemigo (c:_)) = c
celdaAChar (Enemigo []) = '?'

esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _ = False

esMeta :: Celda -> Bool
esMeta META = True
esMeta _ = False

esEnemigo :: Celda -> Bool
esEnemigo (Enemigo _) = True
esEnemigo _ = False
-}
--Subtarea 2

--Subtarea 3
--Bloque 2
--import Data.List (sortOn)
--import Data.Ord (Down(..))
--Subtarea 4
-- import Data.List (stripPrefix)



--No implemento splitOn porque está implementada en la práctica 1
buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (x:xs) =
    case stripPrefix clave x of
        Just valor -> Just valor
        Nothing -> buscarClave clave xs

--Subtarea 5
data ErrorNivel
  = FormatoInvalido String
  | NivelSinMeta
  | NivelConVariasMetas Int
  deriving (Eq, Show)

data NivelData = NivelData
  { nombreNivel :: String
  , fondos      :: [Int]
  , gridNivel   :: Grid
  } deriving (Eq, Show)

--Función parsearFichero, recibe las líneas de un fichero y las lee si están completas o devuelve error
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero lineas = do
    let (cabecera, resto) = break (== "---") lineas
    if null resto
        then Left (FormatoInvalido "Falta el separador ---")
        else do
            let mapa = drop 1 resto

            if null mapa
                then Left (FormatoInvalido "No hay mapa después de ---")
                else do 
                    nombre <- maybe (Left (FormatoInvalido "Falta la clave NOMBRE:")) Right
                                     (buscarClave "NOMBRE: " cabecera)

                    fondosTexto <- maybe (Left (FormatoInvalido "Falta la clave FONDOS:")) Right
                                          (buscarClave "FONDOS: " cabecera)

                    let listaFondos = splitOn ',' fondosTexto

                    fondosLista <- maybe (Left (FormatoInvalido "Fondos no numéricos")) Right
                                          (mapM readMaybe listaFondos)

                    let grid = map (map parsearCelda) mapa

                    return (NivelData nombre fondosLista grid)

--Función validarNivel, ver si un nivel tiene meta o no
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nivel
    | numeroMetas == 0 = Left NivelSinMeta
    | numeroMetas > 1  = Left (NivelConVariasMetas numeroMetas)
    | otherwise       = Right nivel
    where
        numeroMetas = length (filter (== META) (concat (gridNivel nivel)))

--Función parsearNivelCompleto, que se compone de las dos anteriores
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto lineas =
    parsearFichero lineas >>= validarNivel

-- Parsea una lista de niveles, devolviendo el primer error si alguno falla.
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles = traverse parsearNivelCompleto

--Bloque 3
--Subtarea 6
--6A 
tamCelda :: Double
tamCelda = 40

--Calcula la esquina inferior izquierda de la celda i j
posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j =
    (fromIntegral j * tamCelda,
     fromIntegral (total - 1 - i) * tamCelda)

--Recorre las filas y columnas y crea una plataforma cuando encuentra Solido
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas grid =
    [ Plataforma (x, y, tamCelda, tamCelda)
    | (i, fila) <- zip [0..] grid
    , (j, Solido) <- zip [0..] fila
    , let (x, y) = posicionCelda (length grid) i j
    ]

--Como la anterior función pero crea una meta cuando encuentra META
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid grid =
    [ Meta (posicionCelda (length grid) i j)
    | (i, fila) <- zip [0..] grid
    , (j, META) <- zip [0..] fila
    ]

--Busca a los enemigos, los identifica y los centra en su celda 
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid grid =
    [ Enemigo ident
        (x + tamCelda / 2, y + tamCelda / 2)
        (-40, 0)
        (tamCelda, tamCelda)
        Patrulla
    | (i, fila) <- zip [0..] grid
    , (j, InicioEnemigo ident) <- zip [0..] fila
    , let (x, y) = posicionCelda (length grid) i j
    ]

--6B
--Recibe un color 
--import Graphics.Gloss

--6C
--import qualified Graphics.Gloss.Interface.Pure.Game as G

