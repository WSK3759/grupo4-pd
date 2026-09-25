--Gonzalo Gutiérrez Garamendi
import Test.QuickCheck

--Tipos
type Vector = (Double, Double)
type Caja = (Double, Double, Double, Double)
data Lado = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)
type Nivel = [String]
type Posicion = (Int, Int)


--EJERCICIO 1
--Suma de vectores, coordenada por coordenada
sumaVectores :: Vector -> Vector -> Vector
sumaVectores (x1,y1) (x2,y2) = (x1+x2, y1+y2)

--Multiplica un punto por un factor
escalarVector :: Double -> Vector -> Vector
escalarVector n (x1,y1) = (n*x1, n*y1)

--Calcular la distancia euclídea entre dos puntos
distancia :: Vector -> Vector -> Double
distancia (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

--EJERCICIO 2
--Saber si dos cajas delimitadoras se tocan en algún punto
solapan :: Caja -> Caja -> Bool
solapan (x1,y1,z1,q1) (x2,y2,z2,q2) = solapa1 && solapa2
    where
        solapa1 = x1 < x2 + z2 && x1 + z1 > x2 --NO coinciden en el eje X si vale 0
        solapa2 = y1 < y2 + q2 && y1 + q1 > y2 --NO coinciden en el eje Y si vale 0

--EJERCICIO 3
--Si dos cajas colisionan, determinar por qué lado lo hacen más
ladoColision :: Caja -> Caja -> Lado
ladoColision (x1,y1,z1,q1) (x2,y2,z2,q2)
    | (solapePorArriba <= solapePorAbajo) && (solapePorArriba <= solapePorIzquierda) && (solapePorArriba <= solapePorDerecha)
        = Arriba
    | (solapePorAbajo <= solapePorDerecha) && (solapePorAbajo <= solapePorIzquierda)
        = Abajo
    | solapePorIzquierda <= solapePorDerecha
        = Izquierda
    | otherwise
        = Derecha
    where
        solapePorArriba    = (y2+q2) - y1
        solapePorAbajo     = (y1+q1) - y2
        solapePorIzquierda = (x2+z2) - x1
        solapePorDerecha   = (x1+z1) - x2
        

--EJERCICIO 4
--Dividir una cadena en trozos por donde haya un espacio
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
    | c == x    = "" : lista
    | otherwise = (x : head lista) : tail lista
    where
        lista = splitOn c xs

--Eliminar los espacios en blanco, los saltos de tabulador y de línea de una cadena
trim :: String -> String
trim xs = reverse (dropWhile esEspacio (reverse (dropWhile esEspacio xs)))
    where
        esEspacio c = c `elem` [' ', '\t', '\n']

--Contar los elementos de una lista que cumplen una condición
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple p xs = length [x | x <- xs, p x]

--Convertir una lista de dos o mas elementos en un punto/vector 2D
list2Vector2 :: [Double] -> Vector
list2Vector2 [] = error "Lista vacia no posible convertir en Vector2"
list2Vector2 [_] = error "Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) = (x,y)

--EJERCICIO 5
--Convierte las lineas de texto leídas en la estructura de datos adecuada
parsearNivel :: [String] -> Nivel
parsearNivel lineas = lineas

--Indica si una celda es una plataforma sólida
esSolido :: Char -> Bool
esSolido '#' = True
esSolido _   = False

--Indica si una celda es la meta del nivel
esMeta :: Char -> Bool
esMeta 'M' = True
esMeta _   = False

--Indica si una celda está vacía
esVacio :: Char -> Bool
esVacio '.' = True
esVacio _   = False

--Indica si una celda marca el punto de inicio de un enemigo 
esEnemigo :: Char -> Bool
esEnemigo c = not (esSolido c || esMeta c || esVacio c)

--Devuelve las posiciones donde está la meta 
posicionesMeta :: Nivel -> [Posicion]
posicionesMeta nivel = [ (f, c) 
  | (f, fila) <- zip [0..] nivel
  , (c, celda) <- zip [0..] fila
  , esMeta celda 
  ]

--Devuelve las posiciones de los enemigos
posicionesEnemigos :: Nivel -> [Posicion]
posicionesEnemigos nivel = [ (f, c) 
  | (f, fila) <- zip [0..] nivel
  , (c, celda) <- zip [0..] fila
  , esEnemigo celda 
  ]

--Agrupa las columnas '#' consecutivas en pares de tuplas
agruparRachas :: String -> [(Int, Int)]
agruparRachas fila = buscar 0 fila
  where
    buscar :: Int -> String -> [(Int, Int)]
    buscar _ [] = []
    buscar i (c:cs)
      | esSolido c = (i, ancho) : buscar (i + ancho) resto
      | otherwise  = buscar (i + 1) cs
      where
        (racha, resto) = span esSolido (c:cs)
        ancho          = length racha

--EJERCICIO 6
-- Suma conmutativa
prop_suma_conmutativa :: Vector -> Vector -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a
-- +++ OK, passed 100 tests.

-- Sumar en un orden u otro da el mismo resultado
prop_suma_asociativa :: Vector -> Vector -> Vector -> Bool
prop_suma_asociativa a b c = sumaVectores (sumaVectores a b) c == sumaVectores a (sumaVectores b c)
--Failed! Falsified (after 9 tests and 12 shrinks):
--(0.6,0.0)
--(0.3,0.0)
--(4.0,0.0)
--Falla por pequeños errores acumulados al redondear

-- Escalar un vector por 1 no lo cambia
prop_escalar_neutro :: Vector -> Bool
prop_escalar_neutro v = escalarVector 1 v == v
-- +++ OK, passed 100 tests.

-- La distancia entre dos puntos nunca es negativa
prop_distancia_no_negativa :: Vector -> Vector -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0
-- +++ OK, passed 100 tests.

-- La distancia de a a b es igual que de b a a
prop_distancia_simetrica :: Vector -> Vector -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a
-- +++ OK, passed 100 tests.

-- solapan a b es igual a solapan b a
prop_solapan_simetrica :: Caja -> Caja -> Bool
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
