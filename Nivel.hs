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
 

