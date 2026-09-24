--  FORMATO DE ENTREGA:
-- - Declaraciones de tipo (type) situadas al principio del fichero, antes de las funciones
-- - Cada función debe llevar su firma de tipos explícita y un comentario de una línea describiendo qué calcula

-- DECLARACIONES DE TIPO (tipos sinónimos):
-- 1 VECTORES 2D
type Punto2D = (Double, Double) -- A. Definición del tipo sinónimo para un punto/vector 2D (1 Vectores 2D)
-- 2 CAJAS DE COLISION
type CajaColision = (Int, Int, Int, Int) -- B. Definición del tipo sinónimo para una delimitadora (2 Cajas de colisión)
-- 5 PARSEO DEL NIVEL 
type Celda = Char -- C. Definición del tipo sinonimo 'Celda' para representar una celda de un nivel como un carácter
type Nivel = [[Celda]] -- D. Definición del tipo sinonimo 'Nivel' para representar un nivel completo como una serie de Celdas 

-- 1 VECTORES 2D

-- Funcion: sumaVectores --> Dados dos vectores (tipo Punto2D), devuelve su suma componente a componente
sumaVectores :: Punto2D -> Punto2D -> Punto2D
sumaVectores (x1, y1) (x2, y2) = (x1+x2, y1+y2)

-- Funcion: escalarVector --> Multiplica un vector (Punto2D) por un factor escalar, es decir, multiplica un parametro de entrada por cada uno de los componentes del vector
escalarVector :: Double -> Punto2D -> Punto2D
escalarVector n (x, y) = (x * n, y * n)

-- Funcion: distancia --> calcula la distancia Euclidea entre dos vectores (calcula la raiz cuadrada de la suma de los valores absolutos al cuadrado de la resta de 2 vectores (Punto2D) componente a componente
distancia :: Punto2D -> Punto2D -> Double
distancia (x1, y1) (x2, y2) = sqrt (abs (x1 - x2)^2 + abs (y1 - y2)^2)

-- 2 CAJAS DE COLISION

-- Funcion: solapan --> dadas dos cajas delimitadoras, alineadas con los ejes (X e Y), devuelve True en caso de que se solapen en algun punto, False caso contrario
solapan :: CajaColision -> CajaColision -> Bool
solapan (x1, y1, ladoX1, ladoY1) (x2, y2, ladoX2, ladoY2) = (x1 + ladoX1 > x2) && (y1 + ladoY1 > y2) && (x1 < x2 + ladoX2) && (y1 < y2 + ladoY2)


-- 3 LADO DE COLISION

-- Funcion: ladoColision --> dadas dos cajas que YA colisionan, devuelve el lado por el que colisionaron (Arriba, abajo izquierda o derecha), en funcion de por qué lado hay una menor mayor diferencia de posicion
ladoColision :: CajaColision -> CajaColision -> String
ladoColision (x1, y1, ladoX1, ladoY1) (x2, y2, ladoX2, ladoY2)
 | solapeLadoIzquierdo <= solapeLadoDerecho && solapeLadoIzquierdo <= solapeAbajo && solapeLadoIzquierdo <= solapeArriba = "Izquierda"
 | solapeLadoDerecho <= solapeLadoIzquierdo && solapeLadoDerecho <= solapeArriba && solapeLadoDerecho <= solapeAbajo = "Derecha"
 | solapeAbajo <= solapeLadoIzquierdo && solapeAbajo <= solapeLadoDerecho && solapeAbajo <= solapeArriba = "Abajo"
 | otherwise = "Arriba"
 where
    solapeLadoIzquierdo = (x1 + ladoX1) - x2
    solapeLadoDerecho = (x2 + ladoX2) - x1
    solapeAbajo = (y1 + ladoY1) - y2
    solapeArriba = (y2 + ladoY2) - y1

-- 4 UTILIDADES DE LISTAS Y CADENAS

-- Funcion: SplitOn --> dado un caracter separador y una cadena de caracteres, separa la cadena en trozos por cada caracter separador encontrado, luego, retorna una lista de String
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn s (c:cs) 
 | esSeparador c = "" : splitOn s cs
 | otherwise = let (r:rs) = splitOn s cs in (c:r) : rs
 where
    esSeparador x = x == s

-- Función: trim --> Elimina los espacios en blanco (espacios, tabuladores, saltos de línea) al principio y al final de una cadena
trim :: String -> String
trim "" = ""
trim (c:cs)
 | esCharEspecial c = trim cs
 | otherwise = unwords (words cs)
 where 
    esCharEspecial x = x == '\t' || x == '\n'

-- Función: contarSiCumple --> Cuenta cuántos elementos de una lista cumplen una condición dada
contarSiCumple :: (Int -> Bool) -> [Int] -> Int
contarSiCumple _ [] = 0
contarSiCumple f xs = sum [x | x <- xs, f x]

-- Función: list2Vector2 --> Dada una lista de 2 o más numeros, la convierte en un vector/punto 2D; lanza un error si la lista no tiene al menos dos elementos.
list2Vector2 :: [Double] -> Punto2D
list2Vector2 [] = error "Una lista vacía no es posible convertir en Vector2"
list2Vector2 [x] = error "Una lista de un solo elemento no es posible convertir en Vector2"
list2Vector2 (x:y:_) = (x,y)

-- 5 PARSEO DE NIVEL

-- Función: parsearNivel --> Convierte la lista de líneas de texto leídas de un fichero de nivel en la estructura de datos que representa el nivel completo.
parsearNivel :: [String] -> Nivel
parsearNivel [] = []
parsearNivel (s:cs) 
 | s /= [] = s : parsearNivel cs
 | otherwise = parsearNivel cs

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

-- Funcion: esEnemigo --> Indica si una celda marca el punto de inicio de un enemigo (cualquier carácter que no sea sólido, meta ni vacío), con logica de reutilizacion de las funciones anteriores
esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c) && not (esMeta c) && not (esVacio c)

-- Función: posicionesMeta --> Dado un nivel completo, devuelve la lista de posiciones (fila, columna) en las que aparece la meta
posicionesMeta :: Nivel -> [(Int, Int)]
posicionesMeta n = [(f,c) | (f, fila) <- zip [0..] n,
                            (c, caracter) <- zip [0..] fila,
                            caracter == 'M']

-- Función: posicionesEnemigos --> Dado un nivel completo, devuelve la lista de posiciones y el identificador de cada enemigo (fila, columna, identificador).
posicionesEnemigos :: Nivel -> [(Int, Int, Celda)]
posicionesEnemigos nivel = [(f, c, identificador) | (f, fila) <- zip [0..] nivel,
                                    (c, identificador) <- zip [0..] fila,
                                    esEnemigo identificador]

-- Función: agruparRachas --> Dada una fila de un nivel, agrupa las columnas ’#’ consecutivas en pares (columna de inicio, longitud de la racha)
-- uso correcto de span/recursión con acumulador de índice, y manejo
-- correcto de rachas al final de la fila o filas sin sólidos
agruparRachas :: [Celda] -> [(Int, Int)]
agruparRachas fila = posActual 0 fila
 where
    posActual :: Int -> [Celda] -> [(Int, Int)]
    posActual _ [] = []
    posActual posicion (c:f)
     | esSolido c = let (racha, resto) = span (=='#') (c:f)
                        longitud = length racha
                    in (posicion, longitud) : posActual (posicion+longitud) resto
     | otherwise = posActual (posicion+1) f 

-- caso base: el caracter actual NO es solido -> sigo iterando
-- caso recursivo: el caracter actual == '#' -> sumo 1 más al contador en la columna actual
