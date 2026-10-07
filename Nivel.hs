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

posicionesMetaRec :: Grid -> [(Int, Int)]
posicionesMetaRec [] = []
posicionesMetaRec nivel = posicionesMetaRecAux 0 [] nivel -- funcion auxiliar para recursion explícita con acumulador e indice de fila inicializado a 0

posicionesMetaRecAux :: Int -> [(Int, Int)] -> Grid -> [(Int, Int)] -- recibe el indice acumulativo de fila, el acumulador con las posiciones que va encontrando (que tiene mismo tipo que valor de retorno) y el Nivel (Grid) dado.
posicionesMetaRecAux f ac [] = ac 
posicionesMetaRecAux f ac (fila:resto) = posicionesMetaRecAux (f+1) (recorreFila f 0 ac fila) resto
-- en cada llamada recursiva, aumenta el indicador de fila en 1 para iterar por la siguiente fila; recorre la fila actual columna por columna para identificar todas las columnas con Metas; y se llama con el resto del nivel (`resto`),
-- utilizando otra func auxiliar de recursion explicita para recorrer la fila actual 

recorreFila :: Int -> Int -> [(Int, Int)] -> [Celda] -> [(Int, Int)] -- func aux para recorrer fila actual que devuelve todas las coordenadas encontradas añadidas al acumulador de la fila actual, utilizando un indicador de columna para calcular las posiciones
recorreFila f c ac [] = ac -- caso base: el nivel actual es vació -> retorna el valor del ac actual
recorreFila f c ac (celda:restoFila) -- caso recursivo: utilizando patrones para verificar si la primera celda de la fila que estamos procesando es Meta
 | esMeta celda = recorreFila f (c+1) (ac ++ [(f,c)]) restoFila -- esMeta == True -> llamada recursiva aumentando indicador de columna; añadimos al ac la coordenada encontrada; y seguimos la llamada con el resto de la fila
 | otherwise = recorreFila f (c+1) ac restoFila -- esMeta == False -> iteramos por el resto de la fila descartando la coordenada (f,c) actual


-- Función: posicionesEnemigosRec -> igual que posicionesMetaRec, pero para las posiciones e identificadores de los enemigos
posicionesEnemigosRec :: Grid -> [(Int, Int, Celda)]
posicionesEnemigosRec [] = []
posicionesEnemigosRec nivel = posicionesEnemigosRecAux 0 [] nivel

posicionesEnemigosRecAux :: Int -> [(Int, Int, Celda)] -> Grid -> [(Int, Int, Celda)]
posicionesEnemigosRecAux f ac [] = ac
posicionesEnemigosRecAux f ac (fila:resto) = posicionesEnemigosRecAux (f+1) (recorreFilaEnem f 0 ac fila) resto

recorreFilaEnem :: Int -> Int -> [(Int, Int, Celda)] -> [Celda] -> [(Int, Int, Celda)]
recorreFilaEnem f c ac [] = ac
recorreFilaEnem f c ac (celda:restoFilaEnem)
 | esEnemigo celda = recorreFilaEnem f (c+1) (ac ++ [(f,c, celda)]) restoFilaEnem
 | otherwise = recorreFilaEnem f (c+1) ac restoFilaEnem

-- 2 Generalización por recursión de agruparRachas

-- Función: agruparPorCaracter -> Recorre una fila y 
-- agrupa en tríos (columna de inicio, longitud, carácter) las rachas de caracteres consecutivos iguales.

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

simularCaidaLibre :: Int -> Double -> (Double, Double) -> [(Double, Double)]
-- simularCaidaLibre 0 a sInicial = [sInicial]
simularCaidaLibre p a (x, v) = (x,v) : simularCaidaLibreRAux [] p a (x,v)

simularCaidaLibreRAux :: [(Double, Double)] -> Int -> Double -> (Double, Double) -> [(Double, Double)]
simularCaidaLibreRAux ac 0 a (x, v) = ac
simularCaidaLibreRAux ac p a (x, v) = simularCaidaLibreRAux (ac ++ [(x + v, v + a)]) (p-1) a (x + v, v + a)

-- (pos anterior + velocidad anterior, velocidad anterior + aceleracion)

-- 4 Recursión sobre dos listas a la vez (0.5 puntos)

-- Función: zipPropio --> 
    -- Empareja los elementos de dos listas, uno a uno, 
    -- deteniéndose en cuanto una de las dos se acaba

zipPropio :: [a] -> [b] -> [(a,b)]
zipPropio [] _ = []
zipPropio _ [] = []
zipPropio (x:xs) (y:ys) = (x,y) : zipPropio xs ys

-- 5 Recursión con descarte condicional: preparación de líneas

-- Función: esComentarioOVacia --> Dada una línea (lista de caracteres), devuelve un Bool indicando si dicha linea es vacía (" ") o es un comentario (que empiezan por ';')
esComentarioOVacia :: String -> Bool
esComentarioOVacia "" = True
esComentarioOVacia (x:xs)
 | x == ';' = True
 | x == ' ' = esComentarioOVacia xs
 | otherwise = False

-- Función: prepararLineas --> Dada una lista de líneas de fichero de nivel, recorre la lista limpiando el formato: elimina los espacios vacios al principio y al final y las lineas con comentarios
prepararLineas :: [String] -> Grid
prepararLineas = prepararLineasAux []

prepararLineasAux :: Grid -> [String] -> Grid
prepararLineasAux ac [] = reverse ac
prepararLineasAux ac (xs:xss)
 | esComentarioOVacia xs = prepararLineasAux ac xss
 | otherwise = prepararLineasAux (iteraYPrepara xs : ac) xss
 where
    iteraYPrepara :: String -> String
    iteraYPrepara "" = ""
    iteraYPrepara (x:xs) 
     | x /= ' ' = x : iteraYPrepara xs
     | otherwise = iteraYPrepara xs