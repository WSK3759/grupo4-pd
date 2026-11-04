module Objeto ( Objeto(..), 
                Plataforma(..), 
                Meta(..), 
                Enemigo(..), 
                solapan, 
                colisionaCon, 
                colisionaConAlguno 
              ) where 

import Tipos

-- Subtarea 2 - Registros: jugador, enemigos y estado del juego
data Enemigo = Enemigo { identificador :: String, posicionE :: Vector2D, velocidadE :: Vector2D, dimension :: Vector2D, tipoDeEnemigo :: TipoEnemigo} -- 2. Registro para el tipo Enemigo
    deriving (Show, Eq)


-- Subtarea 3 - Una clase de tipos para los objetos del juego

class Objeto a where
  posicion    :: a -> Vector2D
  boundingBox :: a -> BoundingBox
  animar      :: Float -> a -> a
  animar _ = id                    -- método con implementación por defecto

data Plataforma = Plataforma BoundingBox
  deriving (Eq, Show)

data Meta = Meta { posMeta :: Vector2D }
  deriving (Eq, Show)

instance Objeto Enemigo where -- 4. instancia de Objeto a para el tipo Enemigo, con caja de (x - w/2, y - h/2, w, h) y animar dt desplazando al enemigo dt · velocidad (con actualización de registro)
    posicion = posicionE
    boundingBox e = (realToFrac (x - w/2), realToFrac (y - h/2), realToFrac w, realToFrac h)
        where 
            (x, y) = posicionE e
            (w, h) = dimension e
    animar dt e = e {posicionE = sumaVectores (posicionE e) (escalarVector (realToFrac dt) (velocidadE e))}

instance Objeto Plataforma where -- 2. instancia de Objeto a para el tipo Plataforma, que utilza su propia caja como posicion
    posicion (Plataforma (x, y, _, _)) = (realToFrac x, realToFrac y)
    boundingBox (Plataforma caja)      = caja

instance Objeto Meta where -- 3. instancia de Objeto a para Meta, con caja fija de 70x80 en (x,y)
    posicion (Meta pos)       = pos
    boundingBox (Meta (x, y)) = (realToFrac x, realToFrac y, 70, 80)

solapan :: BoundingBox -> BoundingBox -> Bool -- la de la Tarea 1 -- Dadas dos BoundingBox, comprueba si solapan la una con la otra
solapan (x1,y1,ancho1,alto1) (x2,y2,ancho2,alto2) =
    x1 + ancho1 > x2 &&
    x2 + ancho2 > x1 &&
    y1 + alto1 > y2 &&
    y2 + alto2 > y1

colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool -- Dados dos objetos cualesquiera, comprueba si estos dos colisionan
colisionaCon objeto1 objeto2 = solapan (boundingBox objeto1) (boundingBox objeto2)

colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool -- Dados un objeto y una lista de objetos, comprueba si el primer objeto colisiona con alguno de los objetos de la lista
colisionaConAlguno objeto1 = any (colisionaCon objeto1)