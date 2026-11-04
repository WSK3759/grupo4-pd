module Tipos
  ( Vector2D, BoundingBox, sumaVectores, escalarVector
  , Celda(..), Grid, TipoEnemigo(..), EstadoJuego(..)
  , parsearCelda, parsearGrid, celdaAChar, esSolido, esMeta, esEnemigo
  ) where

-- Un vector 2D (x, y); Float porque Gloss trabaja con Float.
type Vector2D = (Float, Float)

-- Caja de colisión (x, y, ancho, alto), con (x, y) en la esquina inferior izquierda.
type BoundingBox = (Float, Float, Float, Float)

-- Suma coordenada a coordenada dos vectores.
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Multiplica las dos coordenadas de un vector por un escalar.
escalarVector :: Float -> Vector2D -> Vector2D
escalarVector k (x, y) = (k * x, k * y)

-- Lo que puede haber en una celda del mapa.
data Celda = Solido | Vacio | META | InicioEnemigo String
  deriving (Eq, Show)

-- Un mapa: lista de filas de celdas (la primera fila es la más alta).
type Grid = [[Celda]]

-- Tipos de enemigo (dinámicas distintas).
data TipoEnemigo = Patrulla | Perseguidor
  deriving (Eq, Show)

-- Situación en la que se encuentra la partida.
data EstadoJuego = Jugando | PierdeVida | GameOver | NivelCompletado
  deriving (Eq, Show)

-- Convierte un carácter del texto del nivel en su celda ('#', '.', 'M'; cualquier otro es un enemigo).
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

-- Convierte las líneas de texto de un mapa en una rejilla de celdas.
parsearGrid :: [String] -> Grid
parsearGrid = map (map parsearCelda)

-- Operación inversa de parsearCelda.
celdaAChar :: Celda -> Char
celdaAChar Solido                = '#'
celdaAChar Vacio                 = '.'
celdaAChar META                  = 'M'
celdaAChar (InicioEnemigo (c:_)) = c
celdaAChar (InicioEnemigo [])    = '?'

-- Indica si la celda es un bloque sólido.
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _      = False

-- Indica si la celda es la meta.
esMeta :: Celda -> Bool
esMeta META = True
esMeta _    = False

-- Indica si la celda es el punto de inicio de un enemigo.
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _                 = False
