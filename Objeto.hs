module Objeto
  ( Objeto(..)
  , Plataforma(..)
  , Meta(..)
  , Enemigo(..)
  , solapan
  , colisionaCon
  , colisionaConAlguno
  ) where

import Tipos

-- SUBTAREA 3
-- Objeto
class Objeto a where
    posicion    :: a -> Vector2D
    boundingBox :: a -> BoundingBox
    animar      :: Float -> a -> a
    animar _ = id                    

data Plataforma = Plataforma BoundingBox
    deriving (Eq, Show)

data Meta = Meta { posMeta :: Vector2D }
    deriving (Eq, Show)

data Enemigo = Enemigo 
    { idEnemigo :: Char
    , posEnemigo :: Vector2D
    , velEnemigo :: Vector2D
    , dimEnemigo :: Vector2D
    , tipoEnemigo :: TipoEnemigo
    } deriving (Eq, Show)

-- instance Objeto Plataforma
instance Objeto Plataforma where
    posicion (Plataforma (x, y, _, _)) = (x, y)
    boundingBox (Plataforma caja) = caja

-- instance Objeto Meta
instance Objeto Meta where
    posicion (Meta p) = p
    boundingBox (Meta (x, y)) = (x, y, 70, 80)

-- instance Objeto Enemigo
instance Objeto Enemigo where
    posicion e = posEnemigo e
    boundingBox e = 
        let (x, y) = posEnemigo e
            (w, h) = dimEnemigo e
        in (x - w / 2, y - h / 2, w, h)
    animar dt e = 
        let desplazamiento = escalarVector dt (velEnemigo e)
            nuevaPos = sumaVectores (posEnemigo e) desplazamiento
        in e { posEnemigo = nuevaPos }

-- solapan
solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1, y1, ancho1, alto1) (x2, y2, ancho2, alto2) =
    x1 + ancho1 > x2 &&
    x2 + ancho2 > x1 &&
    y1 + alto1 > y2 &&
    y2 + alto2 > y1

-- colisionaCon
colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon obj1 obj2 = solapan (boundingBox obj1) (boundingBox obj2)

-- colisionaConAlguno
colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno obj xs = any (colisionaCon obj) xs
