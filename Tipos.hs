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

type Vector2D = (Double, Double)
type BoundingBox = (Double, Double, Double, Double) -- (x, y, ancho, alto)

sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores v1 v2 = (fst v1 + fst v2, snd v1 + snd v2)

escalarVector :: Double -> Vector2D -> Vector2D
escalarVector x v = (x * fst v, x * snd v)

-- TAREA 3
-- Subtarea 1: Celda

data Celda 
    = Solido 
    | Vacio 
    | META 
    | InicioEnemigo String 
    deriving (Eq, Show, Ord)

type Grid = [[Celda]]

-- parsearCelda
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c = InicioEnemigo [c]

-- parsearGrid
parsearGrid :: [String] -> Grid
parsearGrid = map (map parsearCelda)

-- celdaAChar
celdaAChar :: Celda -> Char
celdaAChar Solido = '#'
celdaAChar Vacio = '.'
celdaAChar META = 'M'
celdaAChar (InicioEnemigo s) = head s

-- esSolido
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _ = False

-- esMeta
esMeta :: Celda -> Bool
esMeta META = True
esMeta _ = False

-- esEnemigo
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _ = False

-- SUBTAREA 2: Registros y Estado

data TipoEnemigo 
    = Patrulla 
    | Perseguidor 
    deriving (Eq, Show)

data EstadoJuego 
    = Jugando 
    | PierdeVida 
    | GameOver 
    | NivelCompletado 
    deriving (Eq, Show)

