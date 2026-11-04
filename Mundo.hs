
module Mundo
    ( Mundo(..), mundoInicial
    , actualizarMundo, manejarEvento
    ) where


import Tipos
import Objeto
import Jugador
import Nivel

import Graphics.Gloss.Interface.Pure.Game
------------------------------------------------
-- Bloque 3 - Gloss: del mapa al mundo jugable
------------------------------------------------

-- SUBTAREA 6C - El mundo y su paso de simulacion
-- Definiciones proporcionadas por el profesor.

-- Representa el mundo completo del juego.
data Mundo = Mundo
    { jugadorM     :: Jugador
    , enemigosM    :: [Enemigo]
    , plataformasM :: [Plataforma]
    , metasM       :: [Meta]
    , estadoM      :: EstadoJuego
    , nivelM       :: NivelData
    }
    deriving (Eq, Show)

-- Crea el mundo inicial a partir de los datos de un nivel.
mundoInicial :: NivelData -> Mundo
mundoInicial nd = Mundo
    { jugadorM     = crearJugador (60,120)
    , enemigosM    = enemigosDesdeGrid g
    , plataformasM = gridAPlataformas g
    , metasM       = metasDesdeGrid g
    , estadoM      = Jugando
    , nivelM       = nd
    }
    where
        g = gridNivel nd


-- SUBTAREA 6C - El mundo y su paso de simulacion

-- actualizarMundo
-- Actualiza las posiciones del jugador y los enemigos y comprueba
-- si hay colisiones para determinar el siguiente estado del juego.
actualizarMundo :: Float -> Mundo -> Mundo
actualizarMundo dt mundo
    | estadoM mundo /= Jugando = mundo
    | otherwise = mundo
        {
            jugadorM = jugadorFinal,
            enemigosM = enemigosActualizados,
            estadoM = estadoSiguiente jugadorFinal recibeDano tocaMeta
        }
    where
        -- Movemos el jugador y animamos los enemigos
        jugadorMovido = moverJugador dt (jugadorM mundo)
        enemigosActualizados = map (animar dt) (enemigosM mundo)

        -- Comprobamos si el jugador colisiona con enemigos o metas
        recibeDano = colisionaConAlguno jugadorMovido enemigosActualizados
        tocaMeta = colisionaConAlguno jugadorMovido (metasM mundo)

        -- Si recibe daño, pierde una vida
        jugadorFinal
            | recibeDano = perderVida jugadorMovido
            | otherwise = jugadorMovido


-- manejarEvento
-- Modifica la velocidad del jugador segun la tecla pulsada.
-- Al soltar una flecha anula su componente de velocidad.
-- La tecla r reinicia el mundo.
manejarEvento :: Event -> Mundo -> Mundo
manejarEvento evento mundo =
    case evento of
        -- Flechas pulsadas.
        EventKey (SpecialKey KeyLeft) Down _ _ -> cambiarX (-100)
        EventKey (SpecialKey KeyRight) Down _ _ -> cambiarX 100
        EventKey (SpecialKey KeyUp) Down _ _ -> cambiarY 100
        EventKey (SpecialKey KeyDown) Down _ _ -> cambiarY (-100)

        -- Flechas soltadas.
        EventKey (SpecialKey KeyLeft) Up _ _ -> cambiarX 0
        EventKey (SpecialKey KeyRight) Up _ _ -> cambiarX 0
        EventKey (SpecialKey KeyUp) Up _ _ -> cambiarY 0
        EventKey (SpecialKey KeyDown) Up _ _ -> cambiarY 0

        -- Reiniciamos el mundo con la tecla r.
        EventKey (Char 'r') Down _ _ -> mundoInicial (nivelM mundo)

        -- Los demas eventos no modifican el mundo.
        _ -> mundo

    where
        jugador = jugadorM mundo
        (vx,vy) = velJugador jugador

        -- Cambia solo la velocidad horizontal.
        cambiarX x = mundo
            {
                jugadorM = conVelocidad (x,vy) jugador
            }

        -- Cambia solo la velocidad vertical.
        cambiarY y = mundo
            {
                jugadorM = conVelocidad (vx,y) jugador
            }
