
module Tipos
    ( Vector2D
    , BoundingBox
    , sumaVectores
    , escalarVector
    , Celda(..)
    , Grid
    , TipoEnemigo(..)
    , EstadoJuego(..)
    , parsearCelda
    , parsearGrid
    , celdaAChar
    , esSolido
    , esMeta
    , esEnemigo
    ) where

-- Representa un vector de dos dimensiones.
type Vector2D = (Double, Double)

-- Representa una caja delimitadora.
type BoundingBox = (Double, Double, Double, Double)

-- Suma dos vectores.
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Multiplica un vector por un escalar.
escalarVector :: Double -> Vector2D -> Vector2D
escalarVector k (x, y) = (k * x, k * y)

-- Representa los tipos de celdas que puede contener un nivel.
data Celda
    = Solido
    | Vacio
    | META
    | InicioEnemigo String
    deriving (Eq, Show)

-- Representa una cuadrícula de celdas.
type Grid = [[Celda]]

-- Representa los tipos de enemigos.
data TipoEnemigo = Patrulla | Perseguidor
    deriving (Eq, Show)

-- Representa los posibles estados del juego.
data EstadoJuego
    = Jugando
    | PierdeVida
    | GameOver
    | NivelCompletado
    deriving (Eq, Show)

-- Convierte un carácter en su celda correspondiente.
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

-- Convierte una lista de filas de caracteres en una cuadrícula.
parsearGrid :: [String] -> Grid
parsearGrid = map (map parsearCelda)

-- Convierte una celda en su carácter correspondiente.
celdaAChar :: Celda -> Char
celdaAChar Solido = '#'
celdaAChar Vacio = '.'
celdaAChar META = 'M'
celdaAChar (InicioEnemigo (c:_)) = c
celdaAChar (InicioEnemigo []) = '?'

-- Indica si una celda es sólida.
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _ = False

-- Indica si una celda es una meta.
esMeta :: Celda -> Bool
esMeta META = True
esMeta _ = False

-- Indica si una celda contiene un enemigo.
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _ = False

-- Obtiene todas las posiciones de una cuadrícula en orden por filas.
todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones [] = []
todasLasPosiciones grid =
    (,) <$> [0 .. length grid - 1]
        <*> [0 .. length (head grid) - 1]