--  FORMATO DE ENTREGA:
-- - Declaraciones de tipo (type) situadas al principio del fichero, antes de las funciones
-- - Cada función debe llevar su firma de tipos explícita y un comentario de una línea describiendo qué calcula

-- DECLARACIONES DE TIPO (tipos sinónimos):
-- 1 VECTORES 2D
type Punto2D = (Double, Double) -- A. Definición del tipo sinónimo para un punto/vector 2D (1 Vectores 2D)
-- 2 CAJAS DE COLISION
type CajaColision = (Int, Int, Int, Int) -- B. Definición del tipo sinónimo para una delimitadora (2 Cajas de colisión)


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
splitOn = undefined