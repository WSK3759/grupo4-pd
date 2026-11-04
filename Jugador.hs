
module Jugador
    ( Jugador
    , posicionJ
    , velocidad
    , vidas
    , puntuacion
    , crearJugador
    , perderVida
    , sumarPuntos
    , conVelocidad
    , moverJugador
    , estadoSiguiente
    ) where

import Tipos
import Objeto

-- Representa al jugador con su posición, velocidad, vidas y puntuación.
data Jugador = Jugador
    { posicionJ :: Vector2D
    , velocidad :: Vector2D
    , vidas :: Int
    , puntuacion :: Int
    } deriving (Eq, Show)

-- Define las operaciones de colisión y posición del jugador.
instance Objeto Jugador where
    posicion j = posicionJ j

    boundingBox j =
        let (x, y) = posicionJ j
        in (x - 30, y, 60, 90)

-- Crea un jugador con tres vidas y cero puntos.
crearJugador :: Vector2D -> Jugador
crearJugador pos = Jugador
    { posicionJ = pos
    , velocidad = (0, 0)
    , vidas = 3
    , puntuacion = 0
    }

-- Resta una vida al jugador sin permitir valores negativos.
perderVida :: Jugador -> Jugador
perderVida j = j { vidas = max 0 (vidas j - 1) }

-- Añade puntos a la puntuación del jugador.
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos puntos j = j { puntuacion = puntuacion j + puntos }

-- Cambia la velocidad del jugador.
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad vel j = j { velocidad = vel }

-- Actualiza la posición del jugador según su velocidad y el tiempo transcurrido.
moverJugador :: Double -> Jugador -> Jugador
moverJugador dt j =
    j { posicionJ = sumaVectores (posicionJ j)
          (escalarVector dt (velocidad j)) }

-- Determina el estado del juego según las vidas, el daño y la meta.
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDano tocaMeta
    | vidas j <= 0 = GameOver
    | recibioDano = PierdeVida
    | tocaMeta = NivelCompletado
    | otherwise = Jugando
