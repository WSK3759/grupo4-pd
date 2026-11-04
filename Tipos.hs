

module Tipos
    ( Vector2D, BoundingBox
    , sumaVectores, escalarVector
    , Celda(..), Grid, TipoEnemigo(..), EstadoJuego(..)
    , parsearCelda, parsearGrid, celdaAChar
    , esSolido, esMeta, esEnemigo
    ) where

------------------------------------------------
-- Bloque 1 — Definición de tipos (3.0 puntos)
------------------------------------------------

-- SUBTAREA 1 - Celda como tipo algebraico (1.0 puntos)

-- Representa los distintos tipos de celdas del mapa (antes era type Celda = Char)
data Celda = Vacio
           | Solido
           | META
           | InicioEnemigo String
           deriving (Eq, Show)

-- Representa un nivel como una lista de filas de celdas.
type Grid = [[Celda]]

-- parsearCelda 
-- Convierte un caracter del mapa en una Celda según su tipo
-- Si no es vacio, solido ni meta, se considera el inicio de un enemigo
parsearCelda :: Char -> Celda
parsearCelda '.' = Vacio
parsearCelda '#' = Solido
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

-- parsearGrid
-- Convierte las filas de texto de un nivel en listas de celdas
-- Usamos dos map: uno para recorrer las filas y otro para convertir cada caracter
parsearGrid :: [String] -> Grid
parsearGrid = map (map parsearCelda)

-- celdaAChar
-- Convierte una Celda en el caracter que la representa en el mapa (inversa a la e parsearCelda)
celdaAChar :: Celda -> Char
celdaAChar Vacio = '.'
celdaAChar Solido = '#'
celdaAChar META = 'M'
celdaAChar (InicioEnemigo (c:_)) = c -- Si es un enemigo, devuelve el primer caracter de su identificador
celdaAChar (InicioEnemigo []) = '?'


-- esSolido
-- Comprueba si una celda es solida.
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _ = False

-- esMeta
-- Comprueba si una celda es la meta del nivel
esMeta :: Celda -> Bool
esMeta META = True
esMeta _ = False

-- esEnemigo
-- Comprueba si una celda es el inicio de un enemigo.
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _ = False


-- SUBTAREA 2 - Registros: jugador, enemigos y estado del juego (1.0 puntos)

-- Representamos una posicion o una velocidad en dos dimensiones
type Vector2D = (Float, Float)

-- Suma dos vectores componente a componente (reutilizada para moverJugador)
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- Multiplica las componentes de un vector por un escalar (reutilizada para moverJugador)
escalarVector :: Float -> Vector2D -> Vector2D
escalarVector n (x,y) = (n*x, n*y)

-- Define los dos tipos de enemigos que puede haber.
data TipoEnemigo = Patrulla | Perseguidor
    deriving (Eq, Show)

-- Representa los posibles estados del juego
data EstadoJuego = Jugando
                 | PierdeVida
                 | GameOver
                 | NivelCompletado
                 deriving (Eq, Show)


-- SUBTAREA 3 - Clase de tipos para los objetos del juego

-- Representa una caja de colision mediante su posicion, su ancho y el alto
type BoundingBox = (Float, Float, Float, Float)



