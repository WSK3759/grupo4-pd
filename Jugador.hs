
module Jugador
    ( Jugador
    , posJugador, velJugador, vidas, puntuacion
    , crearJugador, perderVida, sumarPuntos
    , conVelocidad, moverJugador, estadoSiguiente
    ) where


import Tipos
import Objeto (Objeto(..))

------------------------------------------------
-- Bloque 1 - Definicion de tipos (3.0 puntos)
------------------------------------------------

-- SUBTAREA 2 - Registros: jugador, enemigos y estado del juego

-- Representa un jugador con su posicion, velocidad, vidas y puntuacion por sintaxis de registro
data Jugador = Jugador
    { posJugador :: Vector2D
    , velJugador :: Vector2D
    , vidas      :: Int
    , puntuacion :: Int
    }
    deriving (Eq, Show)

-- crearJugador
-- Crea un jugador en la posicion indicada, con velocidad cero,
-- 3 vidas y 0 puntos
crearJugador :: Vector2D -> Jugador
crearJugador pos = Jugador pos (0,0) 3 0

-- perderVida
-- Resta una vida al jugador sin que pueda tener vidas negativas.
perderVida :: Jugador -> Jugador
perderVida j = j 
    { 
        vidas = max 0 (vidas j - 1)  -- contruir un nuevo jugador igual que j pero cambiando el campo vidas
    }

-- sumarPuntos
-- Suma una cantidad de puntos a la puntuacion actual (parecida a perderVida pero actualizando puntuacion)
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos n j = j
    { 
        puntuacion = puntuacion j + n 
    }

-- conVelocidad
-- Cambia la velocidad del jugador.
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad v j = j 
    {
        velJugador = v
    }

-- moverJugador
-- Actualiza la posicion del jugador segun su velocidad
-- y el tiempo transcurrido.
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j = j
    {
        posJugador = sumaVectores (posJugador j) -- sumaVectores calcula nueva posición
        (escalarVector dt (velJugador j)) -- escalaVector calcula el desplazamiento
    }

-- estadoSiguiente
-- Determina el estado del juego segun las vidas,
-- si recibe daño o si llega a la meta.
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDano tocaMeta
    | vidas j <= 0 = GameOver
    | recibioDano = PierdeVida
    | tocaMeta = NivelCompletado
    | otherwise = Jugando


-- SUBTAREA 3 - Instancia Objeto para Jugador

-- La posicion del objeto es la posicion del jugador.
-- Su caja de colision tiene un ancho de 60 y alto de 90
instance Objeto Jugador where
    posicion = posJugador

    boundingBox j = (x-30,y,60,90)
        where
            (x,y) = posJugador j
