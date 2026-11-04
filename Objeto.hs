module Objeto
  ( Objeto(..), Plataforma(..), Meta(..), Enemigo(..)
  , solapan, colisionaCon, colisionaConAlguno
  ) where

import Tipos (BoundingBox, TipoEnemigo, Vector2D, escalarVector, sumaVectores)

-- Todo lo que tiene presencia en el mundo: una posición y una caja de colisión.
class Objeto a where
  posicion    :: a -> Vector2D
  boundingBox :: a -> BoundingBox
  animar      :: Float -> a -> a
  animar _ = id

-- Una plataforma sólida, definida por su caja.
data Plataforma = Plataforma BoundingBox
  deriving (Eq, Show)

-- La meta del nivel, definida por su esquina inferior izquierda.
data Meta = Meta { posMeta :: Vector2D }
  deriving (Eq, Show)

-- Un enemigo con identificador, posición (centro), velocidad, dimensión y tipo.
data Enemigo = Enemigo
  { idEnemigo   :: String
  , posEnemigo  :: Vector2D
  , velEnemigo  :: Vector2D
  , dimEnemigo  :: Vector2D
  , tipoEnemigo :: TipoEnemigo
  } deriving (Eq, Show)

instance Objeto Plataforma where
  posicion (Plataforma (x, y, _, _)) = (x, y)
  boundingBox (Plataforma caja)      = caja

instance Objeto Meta where
  posicion          = posMeta
  boundingBox (Meta (x, y)) = (x, y, 70, 80)

instance Objeto Enemigo where
  posicion = posEnemigo
  boundingBox e = (x - w / 2, y - h / 2, w, h)
    where
      (x, y) = posEnemigo e
      (w, h) = dimEnemigo e
  animar dt e = e { posEnemigo = sumaVectores (posEnemigo e) (escalarVector dt (velEnemigo e)) }

-- Indica si dos cajas de colisión se solapan (de la Tarea 1).
solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1, y1, a1, h1) (x2, y2, a2, h2) =
  x1 < x2 + a2 && x2 < x1 + a1 && y1 < y2 + h2 && y2 < y1 + h1

-- Indica si dos objetos cualesquiera colisionan, comparando sus cajas.
colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon a b = solapan (boundingBox a) (boundingBox b)

-- Indica si un objeto colisiona con alguno de una lista de objetos.
colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno a = any (colisionaCon a)
