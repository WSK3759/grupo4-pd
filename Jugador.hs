module Jugador
  ( Jugador
  , crearJugador
  , perderVida
  , sumarPuntos
  , conVelocidad
  , moverJugador
  , estadoSiguiente
  , posJugador
  , velJugador
  , vidas
  , puntuacion
  ) where

import Tipos
import Objeto

data Jugador = Jugador 
    { posJugador :: Vector2D
    , velJugador :: Vector2D
    , vidas :: Int
    , puntuacion :: Int
    } deriving (Eq, Show)

-- instance Objeto Jugador
instance Objeto Jugador where
    posicion j = posJugador j
    boundingBox j = 
        let (x, y) = posJugador j
        in (x - 30, y, 60, 90)

-- crearJugador
crearJugador :: Vector2D -> Jugador
crearJugador p = Jugador { posJugador = p, velJugador = (0, 0), vidas = 3, puntuacion = 0 }

-- perderVida
perderVida :: Jugador -> Jugador
perderVida j = 
    let vActual = vidas j
        nuevoVidas = if vActual > 0 then vActual - 1 else 0
    in j { vidas = nuevoVidas }

-- sumarPuntos
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos pts j = j { puntuacion = puntuacion j + pts }

-- conVelocidad
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad v j = j { velJugador = v }

-- moverJugador
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j = 
    let desplazamiento = escalarVector dt (velJugador j)
        nuevaPos = sumaVectores (posJugador j) desplazamiento
    in j { posJugador = nuevaPos }

-- estadoSiguiente
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente jugador recibioDaño tocaMeta
    | vidas jugador <= 0 = GameOver
    | recibioDaño = PierdeVida
    | tocaMeta = NivelCompletado
    | otherwise = Jugando
