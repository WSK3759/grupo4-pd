
module Objeto
    ( Objeto(..), Plataforma(..), Meta(..), Enemigo(..)
    , solapan, colisionaCon, colisionaConAlguno
    ) where

import Tipos

------------------------------------------------
-- Bloque 1 - Definicion de tipos (3.0 puntos)
------------------------------------------------

-- SUBTAREA 3 - Una clase de tipos para los objetos del juego

-- Define las operaciones comunes a todos los objetos del juego.
class Objeto a where
  posicion    :: a -> Vector2D
  boundingBox :: a -> BoundingBox
  animar      :: Float -> a -> a
  animar _ = id                    -- método con implementación por defecto


-- SUBTAREA 2 - Registros: jugador, enemigos y estado del juego

-- Representa un enemigo con su identificador, posicion,
-- velocidad, tamaño y tipo de enemigo.
data Enemigo = Enemigo
    { idEnemigo   :: String
    , posEnemigo  :: Vector2D
    , velEnemigo  :: Vector2D
    , tamEnemigo  :: Vector2D
    , tipoEnemigo :: TipoEnemigo
    }
    deriving (Eq, Show)


-- SUBTAREA 3 - Tipos de objetos del juego

-- Representa una plataforma mediante su caja de colision.
data Plataforma = Plataforma BoundingBox
    deriving (Eq, Show)

-- Representa una meta mediante su posicion.
data Meta = Meta
    { posMeta :: Vector2D }
    deriving (Eq, Show)


-- SUBTAREA 3 - Instancias de la clase Objeto

-- Una plataforma tiene como posicion el origen de su caja.
instance Objeto Plataforma where
    posicion (Plataforma (x,y,_,_)) = (x,y)
    boundingBox (Plataforma caja) = caja

-- La meta tiene una caja de tamaño 70x80 (Meta en (x, y) = (x, y, 70, 80))
instance Objeto Meta where
    posicion = posMeta
    boundingBox m = (x,y,70,80)
        where
            (x,y) = posMeta m

-- La caja del enemigo esta centrada en su posicion.
-- Al animarlo actualizamos su posicion usando su velocidad.
instance Objeto Enemigo where
    posicion = posEnemigo

    boundingBox e = (x-w/2, y-h/2, w, h)
        where
            (x,y) = posEnemigo e
            (w,h) = tamEnemigo e

    animar dt e = e
        {
            posEnemigo = sumaVectores (posEnemigo e)
                                      (escalarVector dt (velEnemigo e))
        }


-- SUBTAREA 3 - Funciones de colision

-- solapan
-- Comprueba si dos cajas se solapan en ambos ejes.
-- Reutilizamos el razonamiento de la Tarea 1
solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1,y1,a1,h1) (x2,y2,a2,h2) =
    x1 + a1 > x2 &&
    x2 + a2 > x1 &&
    y1 + h1 > y2 &&
    y2 + h2 > y1

-- colisionaCon
-- Comprueba si dos objetos tienen sus cajas solapadas.
colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon a b = solapan (boundingBox a) (boundingBox b)

-- colisionaConAlguno
-- Comprueba si un objeto colisiona con alguno de una lista dada
colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno objeto lista = any (colisionaCon objeto) lista
