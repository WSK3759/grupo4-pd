module Tipos ( Vector2D, 
               BoundingBox, 
               sumaVectores, 
               escalarVector, 
               Celda(..), 
               Grid, 
               TipoEnemigo(..), 
               EstadoJuego(..), 
               parsearCelda, 
               parsearGrid, 
               celdaAChar, 
               esSolido, 
               esMeta, 
               esEnemigo
            ) where -- Sin importaciones externas ni internas


-- Tipos sinónimos (Tarea 1) -> Modificacion para la TAREA 3: BoundingBox por CajaColision
type Vector2D = (Double, Double)
type BoundingBox = (Float, Float, Float, Float) -- Posición (x, y), Ancho y Alto
type CajaColision = BoundingBox
-- MODIFICACION PARA LA TAREA 3: Subtarea 1 - Celda como tipo algebraico
data Celda = Solido | Vacio | META | InicioEnemigo String
    deriving (Show, Eq)

-- Ahora un Nivel (Grid) es una matriz (lista de listas) de celdas algebraicas
type Grid = [[Celda]]

-- TAREA 1:

sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores v1 v2 = ((fst v1 + fst v2),(snd v1 + snd v2))

escalarVector :: Double -> Vector2D -> Vector2D
escalarVector x v = ((x * fst v),(x * snd v))

-- TAREA 3: BLOQUE 1 — Definición de tipos
-- Subtarea 1 - Celda como tipo algebraico

-- Funcion: parsearCelda - Dado un caracter (Char), lo convierte al tipo Celda correspondiente aplicando pattern matching
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

-- Funcion: parsearGrid - Dado una cadena de caracteres (String), lo convierte al tipo Grid (matriz de celdas)
parsearGrid :: [String] -> Grid
parsearGrid [] = []
parsearGrid (fila:resto) = map parsearCelda fila : parsearGrid resto

-- Funcion: celdaAChar - dado un constructor de tipo algebraico Celda, lo parsea a su tipo Char
celdaAChar :: Celda -> Char
celdaAChar Solido = '#'
celdaAChar Vacio = '.'
celdaAChar META = 'M'
celdaAChar (InicioEnemigo c) = head c

-- Funcion: esSolido - Dado un constructor de tipo Celda, devuelve True si es tipo Solido, False en caso contrario
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _ = False

-- Funcion: esMeta -> Dado un constructor de tipo Celda, devuelve True si es tipo Meta, False en caso contrario
esMeta :: Celda -> Bool
esMeta META = True
esMeta _ = False

-- Funcion: esEnemigo -> Dado un constructor de tipo Enemigo, devuelve True si es tipo Enemigo, False en caso contrario
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _ = False

-- Subtarea 2 - Registros: jugador, enemigos y estado del juego

data TipoEnemigo = Patrulla | Perseguidor -- 1. Dinámica de comportamientos de un enemigo (tipo Enemigo)
    deriving (Show, Eq)

data EstadoJuego = Jugando | PierdeVida | GameOver | NivelCompletado
    deriving (Show, Eq)