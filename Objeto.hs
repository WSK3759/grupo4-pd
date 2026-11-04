
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

-- Representa un enemigo con su identificador, posición, velocidad, dimensiones y tipo.
data Enemigo = Enemigo
    { idEnemigo :: String
    , posicionE :: Vector2D
    , velocidadE :: Vector2D
    , dimEnemigo :: Vector2D
    , tipoE :: TipoEnemigo
    } deriving (Eq, Show)

-- Representa una plataforma.
data Plataforma = Plataforma BoundingBox
    deriving (Eq, Show)

-- Representa la meta del nivel.
data Meta = Meta
    { posMeta :: Vector2D
    } deriving (Eq, Show)

-- Define las operaciones comunes de los objetos del juego.
class Objeto a where
    posicion :: a -> Vector2D
    boundingBox :: a -> BoundingBox
    animar :: Double -> a -> a
    animar _ = id

-- Instancia de Objeto para las plataformas.
instance Objeto Plataforma where
    posicion (Plataforma (x, y, _, _)) = (x, y)
    boundingBox (Plataforma caja) = caja

-- Instancia de Objeto para las metas.
instance Objeto Meta where
    posicion (Meta pos) = pos
    boundingBox (Meta (x, y)) = (x, y, 70, 80)

-- Instancia de Objeto para los enemigos.
instance Objeto Enemigo where
    posicion e = posicionE e

    boundingBox e =
        let (x, y) = posicionE e
            (w, h) = dimEnemigo e
        in (x - w / 2, y - h / 2, w, h)

    animar dt e =
        e { posicionE = sumaVectores (posicionE e)
                (escalarVector dt (velocidadE e)) }

-- Comprueba si dos cajas se solapan.
solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1,y1,ancho1,alto1) (x2,y2,ancho2,alto2) =
    x1 + ancho1 > x2 &&
    x2 + ancho2 > x1 &&
    y1 + alto1 > y2 &&
    y2 + alto2 > y1

-- Comprueba si dos objetos colisionan.
colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon objeto1 objeto2 =
    solapan (boundingBox objeto1) (boundingBox objeto2)

-- Comprueba si un objeto colisiona con alguno de una lista.
colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno objeto = any (colisionaCon objeto)
