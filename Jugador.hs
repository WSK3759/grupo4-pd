module Jugador ( Jugador, -- ¡ATENCIÓN! Oculta el constructor (sin (..)) 
                 posicionJ, 
                 velocidadJ, 
                 vidas, 
                 puntuacion, 
                 crearJugador, 
                 perderVida, 
                 sumarPuntos , 
                 conVelocidad , 
                 moverJugador , 
                 estadoSiguiente 
               ) where 

import Tipos 
import Objeto

data Jugador = Jugador { posicionJ :: Vector2D, velocidadJ :: Vector2D, vidas :: Int, puntuacion :: Int} -- 1. Registro para el tipo Jugador
    deriving (Show, Eq)

instance Objeto Jugador where -- 1. Instancia de Objeto para el tipo Jugador (cajaColision de 60x90 con pos en x en x-30) 
    posicion = posicionJ
    boundingBox j = (realToFrac x - 30, realToFrac y, 60, 90)
        where (x, y) = posicionJ j

-- Funcion: crearJugador - Dada una posicion, crear el jugador con el resto de campos predeterminados y la posicion dada
crearJugador :: Vector2D -> Jugador
crearJugador pos = Jugador {posicionJ = pos, velocidadJ = (0,0), vidas = 3, puntuacion = 0}

-- Funcion: perderVida - Dado un jugador, resta una vida al numero de vidas que tiene y devuelve el jugador actualizado
perderVida :: Jugador -> Jugador 
perderVida jugador = jugador {vidas = max 0 (vidas jugador - 1)}

-- Funcion: sumarPuntos - Dado un numero de puntos y un jugador, actualiza el jugador sumando la puntuacion que tenia el nuevo puntaje dado
sumarPuntos :: Int -> Jugador -> Jugador 
sumarPuntos p jugador = jugador {puntuacion = puntuacion jugador + p}

-- Funcion: conVelocidad - Dada una velocidad y un jugador, actualiza su velocidad con la velocidad dada
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad nV jugador = jugador {velocidadJ = nV}

-- Funcion: moverJugador - Dado un Float dt (desplazamiento), calcula la nueva posicion del jugador como su posicion original + dt * velocidad
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt jugador = jugador {posicionJ = sumaVectores (posicionJ jugador) (escalarVector (realToFrac dt) (velocidadJ jugador))}

-- Funcion: estadoSiguiente - Dado un jugador, y dos booleanos que indican si ha recinbido daño y ha tocado meta, cambia el estado del juego según la reglas del enunciado
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDaño tocaMeta
    | vidas j <= 0                             = GameOver
    | recibioDaño                              = PierdeVida
    | tocaMeta                                 = NivelCompletado
    | otherwise                                = Jugando