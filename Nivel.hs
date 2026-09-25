----------------------------------------------------------
-- Tarea 1
-- Lógica pura del motor de niveles y colisiones
-- Programación Declarativa — Curso 2026/2027
----------------------------------------------------------
import Test.QuickCheck
----------------------------------------------------------
-- 1 Vectores 2D
----------------------------------------------------------

-- Definir un tipo sinónimo para un punto/*vector 2D*
type Vector2D = (Double, Double)


-- Función: sumaVectores
-- Suma componente a componente dos vectores/puntos 2D.

sumaVectores :: Vector2D -> Vector2D -> Vector2D -- recibe dos vectores y devuelve otro
sumaVectores (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)


-- Función: escalarVector
-- Multiplica un vector/punto 2D por un factor escalar.

escalarVector :: Double -> Vector2D -> Vector2D
escalarVector d (x,y) = (d * x, d * y)


-- Función: distancia
-- Calcula la distancia euclídea entre dos puntos 2D

distancia :: Vector2D -> Vector2D -> Double
distancia (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)


----------------------------------------------------------
-- 2 Cajas de colisión
----------------------------------------------------------

--  Definición correcta del tipo sinónimo para una *caja* delimitadora
type Caja = (Double, Double, Double, Double)


-- Función: solapan
-- Indica si dos cajas delimitadoras, alineadas con los ejes, se solapan en algún punto -- (0,0,10,10) (5,5,10,10)
-- Si solo se tocan en el borde, no se consideran solapadas

solapan :: Caja -> Caja -> Bool
solapan (x1,y1,ancho1,alto1) (x2,y2,ancho2,alto2) = 
    x1 + ancho1 > x2 &&
    x2 + ancho2 > x1 &&
    y1 + alto1 > y2 &&
    y2 + alto2 > y1


----------------------------------------------------------
-- 3 Lado de colisión
----------------------------------------------------------

-- Dadas dos cajas que ya colisionan, se pide determinar por qué lado es más superficial el
-- solape (ese es el lado por el que efectivamente se produjo el contacto).

-- Función: ladoColision
-- Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto (el de
-- menor solape) (usar where y guardas)

ladoColision :: Caja -> Caja -> String
ladoColision (x1,y1,ancho1,alto1) (x2,y2,ancho2,alto2)
    | solapeArriba <= solapeAbajo 
    && solapeArriba <= solapeIzquierda 
    && solapeArriba <= solapeDerecha = "Arriba"
    
    | solapeAbajo <= solapeArriba && 
    solapeAbajo <= solapeIzquierda && 
    solapeAbajo <= solapeDerecha = "Abajo"
    
    | solapeIzquierda <= solapeAbajo 
    && solapeIzquierda <= solapeArriba 
    && solapeIzquierda <= solapeDerecha = "Izquierda"
    
    | otherwise = "Derecha"

    where
        solapeArriba = y2 + alto2 - y1 -- Solape de la caja 1 entrando por arriba
        solapeAbajo = y1 + alto1 - y2
        solapeIzquierda = x1 + ancho1 - x2
        solapeDerecha = x2 + ancho2 - x1


----------------------------------------------------------
-- 4 Utilidades de listas y cadenas
----------------------------------------------------------

-- Función: splitOn
-- Divide una cadena en trozos cada vez que aparece un carácter separador dado.
-- vía pattern matching x:xs

-- si aparece el separador... empiezo un nuevo trozo, sino, añado el carácter al trozo actual
-- un sumaLista, pero construyendo trozos en vez de sumando números
splitOn :: Char -> String -> [String]
splitOn separador [] = [""] -- caso base
splitOn separador (x:xs)
    | x == separador = "" : splitOn separador xs   -- si x es el separador, creo un nuevo trozo vacío y sigo con el resto
    | otherwise = (x:y) : ys
    where
        (y:ys) = splitOn separador xs

-- Función: trim
-- Elimina los espacios en blanco (espacios, tabuladores, saltos de línea) al principio y al final
-- de una cadena.

trim :: String -> String
trim [] = [] -- caso base
trim (x:xs)
    | (x == ' ' || x == '\t' || x == '\n') = trim xs
    | y == ' ' || y == '\t' || y == '\n' = reverse (trim ys) -- (where) ... y hacemos la misma operación
    | otherwise = x:xs -- por si no hay espacios (las dos primeras guardas False)
    where
        (y:ys) = reverse (x:xs) -- le damos la vuelta al String original para eliminar los espacios en blanco del final


-- Función: contarSiCumple
-- Cuenta cuántos elementos de una lista cumplen una condición dada

-- resuelta obligatoriamente con lista por comprensión, no con foldr/map

contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple condicion xs = length [x | x <- xs, condicion x]


-- Función: list2Vector2
-- Convierte una lista de dos (o más) números en un vector/punto 2D; lanza un error si la lista
-- no tiene al menos dos elementos.

--  list2Vector2, con pattern matching en los casos [], [x], (x:y:_) y uso de error en los
-- casos inválidos; el tipo del resultado debe coincidir con el definido en la Subtarea 1

list2Vector2 :: [Double] -> Vector2D
list2Vector2 [] = error "Lista vacia no posible convertir en Vector2" -- no tiene al menos dos elementos
list2Vector2 [x] = error "La lista no tiene al menos dos elementos"
list2Vector2 (x:y:_) = (x,y)


----------------------------------------------------------
-- 5 Parseo del nivel
----------------------------------------------------------

type Celda = Char
type Nivel = [String]

-- Función: parsearNivel
-- Convierte la lista de líneas de texto leídas de un fichero de nivel en la estructura de datos
-- que representa el nivel completo

parsearNivel :: [String] -> Nivel
parsearNivel lineas = lineas

-- usar pattern matching directo:

-- Función: esSolido
-- Indica si una celda es una plataforma sólida.

esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _ = False

-- Función: esMeta
-- Indica si una celda es la meta del nivel.

esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _ = False

-- Función: esVacio
-- Indica si una celda está vacía (no hay nada en ella).

esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _ = False

-- Función: esEnemigo
-- Indica si una celda marca el punto de inicio de un enemigo (cualquier carácter que no sea
-- sólido, meta ni vacío).

esEnemigo :: Celda -> Bool
esEnemigo celda = not (esSolido celda || esMeta celda || esVacio celda)

-- Función: posicionesMeta
-- Dado el nivel completo, devuelve la lista de posiciones (fila, columna) en las que aparece la
-- meta. fila -> i columna -> j

posicionesMeta :: Nivel -> [(Int, Int)] -- Nivel -> lista de (fila, columna)
posicionesMeta nivel =
    [(i,j) |
        (i,fila) <- zip [0..] nivel,
        (j,celda) <- zip [0..] fila,
        esMeta celda -- filtra solo las 'M'
    ]

-- Función: posicionesEnemigos
-- Dado el nivel completo, devuelve la lista de posiciones y el identificador de cada enemigo
-- (fila, columna, identificador).
-- (1,2,"X") "X" es String no char

posicionesEnemigos :: Nivel -> [(Int, Int, String)]
posicionesEnemigos nivel =
    [(i,j,[celda]) |
        (i,fila) <- zip [0..] nivel,
        (j,celda) <- zip [0..] fila,
        esEnemigo celda
    ]

-- Función: agruparRachas
-- Recorre una fila del nivel y agrupa las columnas ’#’ consecutivas en pares (columna de inicio,
-- longitud de la racha)
-- > agruparRachas "..###.##"
-- [(2,3),(6,2)]

agruparRachas :: String -> [(Int, Int)]
agruparRachas fila = agrupar fila 0
    where
        agrupar :: String -> Int -> [(Int, Int)]

        agrupar [] _ = []

        agrupar (x:xs) indice
            | esSolido x = (indice, length consecutivos) : agrupar resto (indice + length consecutivos)
            | otherwise = agrupar xs (indice + 1)
            where
                (consecutivos, resto) = span esSolido (x:xs)

-- Bonus: Propiedades con QuickCheck

prop_suma_conmutativa :: Vector2D -> Vector2D -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

prop_suma_asociativa :: Vector2D -> Vector2D -> Vector2D -> Bool
prop_suma_asociativa a b c = sumaVectores (sumaVectores a b) c == sumaVectores a (sumaVectores b c)

prop_escalar_neutro ::  Vector2D -> Bool
prop_escalar_neutro vector = escalarVector 1 vector == vector

prop_distancia_no_negativa :: Vector2D -> Vector2D -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0

prop_distancia_simetrica :: Vector2D -> Vector2D -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a

prop_solapan_simetrica :: Caja -> Caja -> Bool
prop_solapan_simetrica a b = solapan a b == solapan b a

prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s

prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador separador xs =
    separador `notElem` xs ==> splitOn separador xs == [xs]

prop_contarSiCumple_acotado :: (Int -> Bool) -> [Int] -> Bool
prop_contarSiCumple_acotado condicion xs =
    contarSiCumple condicion xs <= length xs
