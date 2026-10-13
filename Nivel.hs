type Abscisas = Double
type Ordenadas = Double
type Ancho = Double
type Alto = Double
-- ##############################################################################
-- Cargar con ghci :l Nivel.hs guardar y recargar con :r

-- Ejercicio 1 Vectores 2D
type Vector2D = (Abscisas, Ordenadas)

sumaVectores :: Vector2D -> Vector2D -> Vector2D -- Suma por separado cada coordenada y crea un nuevo vector
sumaVectores p s = ( fst p + fst s, snd p + snd s ) 

escalarVectores :: Double -> Vector2D -> Vector2D -- Multiplica ambas coordenadas de un vector por un escalar y crea uno nuevo
escalarVectores n v = ( fst v * n, snd v * n )

distancia :: Vector2D -> Vector2D -> Double    -- La función calcula por pitagoras la distancia entre puntos
distancia p s =   sqrt ( ( fst p - fst s) ^ 2 + ( snd p - snd s ) ^ 2 )


-- Ejercicio 2 Cajas de Colisión 
type Caja = (Abscisas, Ordenadas, Ancho, Alto)

solapan :: Caja -> Caja -> Bool
solapan (x1, y1, a1, h1) (x2, y2, a2, h2) = 
    x1 < x2 + a2 &&
    x2 < x1 + a1 &&
    y1 < y2 + h2 &&
    y2 < y1 + h1


-- Ejercicio 3 Lado de Colisión


ladoColision :: Caja -> Caja -> String
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

-- 4 Utilidades de listas y cadenas

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn separador (x:xs)
    | x == separador = "" : splitOn separador xs
    | otherwise = (x : primera) : resto
    where
        (primera:resto) = splitOn separador xs


trim :: String -> String
trim = quitarFinal . quitarInicio
    where
        quitarInicio [] = []
        quitarInicio (x:xs)
            | x == ' ' || x == '\t' || x == '\n' = quitarInicio xs
            | otherwise = x:xs

        quitarFinal xs = reverse (quitarInicio (reverse xs))


-- Poner explicaciones de los codigos

contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple _ [] = 0
contarSiCumple condicion (x:xs)
    | condicion x = 1 + contarSiCumple condicion xs
    | otherwise = contarSiCumple condicion xs 


list2Vector2 :: [Float] -> (Float, Float)
list2Vector2 [] = error "Lista vacia no posible convertir en Vector2"
list2Vector2 [_] = error "Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) = (x, y)


-- 5 Parseo del Nivel

type Nivel = [[Celda]]
type Celda = Char

parsearNivel :: [String] -> Nivel
parsearNivel s = s

esSolido :: Celda -> Bool
esSolido c 
    | c == '#' = True
    | otherwise = False

esMeta :: Celda -> Bool
esMeta c 
    | c == 'M' = True
    | otherwise = False

esVacio :: Celda -> Bool
esVacio c
    | c == '.' = True
    | otherwise = False

esEnemigo :: Celda -> Bool
esEnemigo c
    | c == '.' || c == 'M' || c == '#' = False
    | otherwise = True

posicionesMeta :: Nivel -> [(Int, Int)]
posicionesMeta nivel = [(x, y) 
                    | (fila, x) <- zip nivel [0..],
                    (celda, y) <- zip fila [0..],
                    esMeta celda]
                    
posicionesEnemigos :: Nivel -> [(Int, Int, Char)]
posicionesEnemigos nivel = [(x, y, celda) 
                    | (fila, x) <- zip nivel [0..],
                    (celda, y) <- zip fila [0..],
                    esEnemigo celda]




agruparRachas :: String -> [(Int, Int)]
agruparRachas xs = aux xs 0
    where
        aux [] _ = []
        aux (x:xs) pos
            | esSolido x = (pos, contarRacha xs 1) : aux (saltarRacha xs) (pos + contarRacha xs 1)
            | otherwise = aux xs (pos + 1)

        contarRacha [] n = n
        contarRacha (x:xs) n
            | x == '#' = contarRacha xs (n + 1)
            | otherwise = n

        saltarRacha [] = []
        saltarRacha (x:xs)
            | x == '#' = saltarRacha xs
            | otherwise = x:xs
 

-- ============================================================
-- TAREA 2: añadir a continuación del Nivel.hs de la Tarea 1
-- Se asume que ya existen esSolido :: Char -> Bool y esVacio :: Char -> Bool
-- ============================================================

-- ------------------------------------------------------------
-- BLOQUE 1: RECURSIÓN
-- ------------------------------------------------------------

-- Posiciones (fila, columna) de la meta, con recursión explícita.
posicionesMetaRec :: [String] -> [(Int, Int)]
posicionesMetaRec nivel = recorrerFilas 0 nivel
  where
    recorrerFilas _ [] = []
    recorrerFilas f (fila:resto) = recorrerColumnas f 0 fila ++ recorrerFilas (f + 1) resto

    recorrerColumnas _ _ [] = []
    recorrerColumnas f c (x:xs)
      | x == 'M'  = (f, c) : recorrerColumnas f (c + 1) xs
      | otherwise = recorrerColumnas f (c + 1) xs

-- Posiciones e identificador de los enemigos (letras mayúsculas salvo la meta 'M'), con recursión explícita.
posicionesEnemigosRec :: [String] -> [(Int, Int, String)]
posicionesEnemigosRec nivel = recorrerFilas 0 nivel
  where
    recorrerFilas _ [] = []
    recorrerFilas f (fila:resto) = recorrerColumnas f 0 fila ++ recorrerFilas (f + 1) resto

    recorrerColumnas _ _ [] = []
    recorrerColumnas f c (x:xs)
      | esEnemigoChar x = (f, c, [x]) : recorrerColumnas f (c + 1) xs
      | otherwise       = recorrerColumnas f (c + 1) xs

    esEnemigoChar x = x >= 'A' && x <= 'Z' && x /= 'M'

-- Agrupa en (columna de inicio, longitud, carácter) las rachas de caracteres iguales de una fila.
agruparPorCaracter :: String -> [(Int, Int, Char)]
agruparPorCaracter fila = agrupar 0 fila
  where
    agrupar _ [] = []
    agrupar i (c:cs) = (i, longitud, c) : agrupar (i + longitud) resto
      where
        (iguales, resto) = span (== c) cs
        longitud = 1 + length iguales

-- Un paso de caída libre: la posición avanza con la velocidad actual y la velocidad con la gravedad.
pasoCaida :: Double -> (Double, Double) -> (Double, Double)
pasoCaida g (pos, vel) = (pos + vel, vel + g)

-- Lista de los n+1 estados (posición, velocidad) de una caída libre con aceleración g.
simularCaidaLibre :: Int -> Double -> (Double, Double) -> [(Double, Double)]
simularCaidaLibre n g estado
  | n <= 0    = [estado]
  | otherwise = estado : simularCaidaLibre (n - 1) g (pasoCaida g estado)

-- Empareja dos listas elemento a elemento, parando cuando se acaba la más corta.
zipPropio :: [a] -> [b] -> [(a, b)]
zipPropio (x:xs) (y:ys) = (x, y) : zipPropio xs ys
zipPropio _      _      = []

-- Quita los espacios en blanco (espacio, tabulador, saltos) de ambos extremos de una línea.
recortar :: String -> String
recortar = reverse . dropWhile esEspacio . reverse . dropWhile esEspacio
  where esEspacio c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

-- Indica si una línea está vacía (o solo espacios) o es un comentario (empieza por ';').
esComentarioOVacia :: String -> Bool
esComentarioOVacia linea = case recortar linea of
  []      -> True
  (c : _) -> c == ';'

-- Recorta cada línea y descarta las vacías y los comentarios, con recursión explícita.
prepararLineas :: [String] -> [String]
prepararLineas [] = []
prepararLineas (l:ls)
  | esComentarioOVacia l = resto
  | otherwise            = recortar l : resto
  where resto = prepararLineas ls

-- ------------------------------------------------------------
-- BLOQUE 2: FUNCIONES DE ORDEN SUPERIOR
-- ------------------------------------------------------------

-- Aplica una función a cada elemento de la lista, manteniendo el orden.
mapPropio :: (a -> b) -> [a] -> [b]
mapPropio _ []     = []
mapPropio f (x:xs) = f x : mapPropio f xs

-- Se queda con los elementos de la lista que cumplen la condición.
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ [] = []
filterPropio p (x:xs)
  | p x       = x : filterPropio p xs
  | otherwise = filterPropio p xs

-- Pliegue por la derecha: f x1 (f x2 (... (f xn z))).
foldrPropio :: (a -> b -> b) -> b -> [a] -> b
foldrPropio _ z []     = z
foldrPropio f z (x:xs) = f x (foldrPropio f z xs)

-- Pliegue por la izquierda con acumulador: f (... (f (f z x1) x2) ...) xn.
foldlPropio :: (b -> a -> b) -> b -> [a] -> b
foldlPropio _ acc []     = acc
foldlPropio f acc (x:xs) = foldlPropio f (f acc x) xs

-- Indica si dos cajas (x, y, ancho, alto) se solapan (auxiliar; usa tu detector de colisión de la Tarea 1 si lo prefieres).
cajasSolapan :: (Double, Double, Double, Double) -> (Double, Double, Double, Double) -> Bool
cajasSolapan (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1

-- Indica si la caja colisiona con alguna de la lista.
-- Justificación: se usa foldr porque combina con (||), que es perezoso en su segundo argumento:
-- en cuanto una caja colisiona, foldr no evalúa el resto de la lista (cortocircuito), cosa que
-- foldl no puede hacer porque debe recorrer toda la lista para devolver el acumulador final.
hayColisionConAlguna :: (Double, Double, Double, Double) -> [(Double, Double, Double, Double)] -> Bool
hayColisionConAlguna caja obstaculos = foldr combina False obstaculos
  where combina obstaculo resto = cajasSolapan caja obstaculo || resto

-- Cuenta las celdas del nivel que cumplen la condición.
-- Justificación: se usa foldl porque es una acumulación numérica simple: se arrastra un contador
-- desde el principio, sin cortocircuito posible (hay que mirar todas las celdas) ni estructura
-- que reconstruir, que es lo que sí justificaría foldr.
contarCeldasQueCumplen :: (Char -> Bool) -> [String] -> Int
contarCeldasQueCumplen cond nivel = foldl sumaFila 0 nivel
  where
    sumaFila acc fila = acc + contarFila fila
    contarFila fila   = foldl cuenta 0 fila
    cuenta acc c
      | cond c    = acc + 1
      | otherwise = acc
