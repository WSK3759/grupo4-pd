module Jugador
  ( Jugador, posJugador, velJugador, vidas, puntuacion
  , crearJugador, perderVida, sumarPuntos, conVelocidad, moverJugador, estadoSiguiente
  ) where

import Objeto (Objeto(..))
import Tipos (EstadoJuego(..), Vector2D, escalarVector, sumaVectores)

-- El jugador (el constructor NO se exporta: solo se crea con crearJugador).
data Jugador = Jugador
  { posJugador :: Vector2D
  , velJugador :: Vector2D
  , vidas      :: Int
  , puntuacion :: Int
  } deriving (Eq, Show)

instance Objeto Jugador where
  posicion = posJugador
  boundingBox j = (x - 30, y, 60, 90)
    where (x, y) = posJugador j

-- Crea un jugador en la posición dada, parado, con 3 vidas y 0 puntos.
crearJugador :: Vector2D -> Jugador
crearJugador p = Jugador { posJugador = p, velJugador = (0, 0), vidas = 3, puntuacion = 0 }

-- Resta una vida sin bajar nunca de 0.
perderVida :: Jugador -> Jugador
perderVida j = j { vidas = max 0 (vidas j - 1) }

-- Suma puntos a la puntuación del jugador.
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos n j = j { puntuacion = puntuacion j + n }

-- Cambia la velocidad del jugador.
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad v j = j { velJugador = v }

-- Avanza la posición dt segundos con la velocidad actual (pos + dt * vel).
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j = j { posJugador = sumaVectores (posJugador j) (escalarVector dt (velJugador j)) }

-- Estado que corresponde tras un paso: GameOver > PierdeVida > NivelCompletado > Jugando.
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDanio tocaMeta
  | vidas j <= 0 = GameOver
  | recibioDanio = PierdeVida
  | tocaMeta     = NivelCompletado
  | otherwise    = Jugando
