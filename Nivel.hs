import Test.QuickCheck

-- Tipos
type Vector2D = (Double, Double)
type CajaColision = (Double, Double, Double, Double) -- Posición (x, y), Ancho y Alto
type Celda = Char
type Grid = [String] -- String de caracteres Celda, grid = [[Celda]]

-- 1. Vectores 2D
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores v1 v2 = ((fst v1 + fst v2),(snd v1 + snd v2))

escalarVector :: Double -> Vector2D -> Vector2D
escalarVector x v = ((x * fst v),(x * snd v))

distancia :: Vector2D -> Vector2D -> Double
distancia v1 v2 = sqrt((fst v1 - fst v2)^2 + (snd v1 - snd v2)^2)

-- 2. Cajas de colisión
solapan :: CajaColision -> CajaColision -> Bool
solapan c1 c2 = (x1 < x2 + w2) && (y1 < y2 + h2) && (x2 < x1 + w1) && (y2 < y1 + h1) 
    where
      (x1, y1, w1, h1) = c1
      (x2, y2, w2, h2) = c2

-- 3. Lado de colisión  

ladoColision :: CajaColision -> CajaColision -> String
ladoColision (x1, y1, w1, h1) (x2, y2, w2, h2) 
    | minSolape == solapeArriba = "Arriba"
    | minSolape == solapeAbajo = "Abajo"
    | minSolape == solapeDerecha = "Derecha"
    | otherwise = "Izquierda"

    where
    solapeArriba    = (y2 + h2) - y1
    solapeAbajo     = (y1 + h1) - y2
    solapeIzquierda = (x1 + w1) - x2
    solapeDerecha = (x2 + w2) - x1
    minSolape = min (min solapeArriba solapeAbajo) (min solapeIzquierda solapeDerecha)

-- 4. Utilidades de listas y cadenas 

-- Función splitOn
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn cSep (x:xs)
  | cSep == x = "" : resto
  | otherwise = (x : head resto) : tail resto
  where
    resto = splitOn cSep xs

-- Función trim

esEspacio :: Char -> Bool
esEspacio c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

trimLeft :: String -> String
trimLeft "" = ""                  
trimLeft (c:cs)
  | esEspacio c = trimLeft cs
  | otherwise   = c:cs

trim :: String -> String
trim s = reverse (trimLeft (reverse (trimLeft s)))

-- Función contarSiCumple

contarSiCumple:: (Int -> Bool) -> [Int] -> Int
contarSiCumple c xs = length cumplenCond
    
    where 
      cumplenCond = [ x | x <- xs, c x]

-- Función: list2Vector2

list2Vector2 :: [Double] -> Vector2D
list2Vector2 [] = error "head : Lista vacia no posible convertir en Vector2"  
list2Vector2 [x]  = error "head : Falta un elemento en la lista para convertir en Vector2" 
list2Vector2 (x:y:_) = (x, y)

-- 5. Parseo del nivel
-- Función: parsearNivel
parsearNivel :: [String] -> Grid
parsearNivel lineas = lineas 

-- Función: esSolido
esSolido :: Celda -> Bool 
esSolido '#' = True
esSolido _ = False

--Función: esMeta
esMeta :: Celda -> Bool 
esMeta 'M' = True
esMeta _ = False

--Función: esVacio
esVacio :: Celda -> Bool 
esVacio '.'= True
esVacio _ = False

--Función: esEnemigo
esEnemigo :: Celda -> Bool 
esEnemigo c = not (esSolido c) && not (esMeta c) && not (esVacio c)

--Función: posicionesMeta
posicionesMeta :: Grid -> [Vector2D]
posicionesMeta nivel = 
    [(indiceLista, indiceString)
    |(indiceLista, stringLista) <- zip [0..] nivel -- aqui voy a tenr un indice asociado a una string [(0, "hola"), (1,"huevo")]
    ,(indiceString, caracter) <- zip[0..] stringLista -- [(0, 'h'),(1,'o') ...]
    , esMeta caracter 
    ]

-- Función: posicionesEnemigos
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


-- Bonus: Propiedades con QuickCheck 

-- Suma conmutativa
prop_suma_conmutativa :: Vector2D -> Vector2D -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- Suma asociativa : -- QuickCheck encuentra un contraejemplo porque al sumar decimales (Double) 
-- el ordenador acumula pequeños errores de redondeo. Aunque matemáticamente es 
-- asociativa, la igualdad estricta (==) falla por milésimas.

prop_suma_asociativa :: Vector2D -> Vector2D -> Vector2D -> Bool
prop_suma_asociativa a b c = sumaVectores (sumaVectores a b) c == sumaVectores a (sumaVectores b c)

-- Escalar neutro
prop_escalar_neutro :: Vector2D -> Bool
prop_escalar_neutro v = escalarVector 1.0 v == v

-- Distancia no negativa
prop_distancia_no_negativa :: Vector2D -> Vector2D -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0

-- Distancia simétrica
prop_distancia_simetrica :: Vector2D -> Vector2D -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a

-- Solapan simétrica
prop_solapan_simetrica :: CajaColision -> CajaColision -> Bool
prop_solapan_simetrica a b = solapan a b == solapan b a

-- Trim idempotente
prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s

-- SplitOn sin separador: no sé hacerlo

-- ContarSiCumple acotado
-- 9. prop_contarSiCumple_acotado
prop_contarSiCumple_acotado :: [Int] -> Bool
prop_contarSiCumple_acotado xs = contarSiCumple even xs <= length xs
