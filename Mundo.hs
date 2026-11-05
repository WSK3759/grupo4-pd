
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
actualizarMundo dt m
  | estadoM m /= Jugando = m
  | otherwise = m
      { jugadorM  = jugadorFinal
      , enemigosM = enemigos'
      , estadoM   = estadoSiguiente jugadorFinal hayDanio tocaMeta
      }
  where
    jugadorMovido = moverJugador dt (jugadorM m)
    enemigos'     = map (animar dt) (enemigosM m)
    hayDanio      = colisionaConAlguno jugadorMovido enemigos'
    tocaMeta      = colisionaConAlguno jugadorMovido (metasM m)
    jugadorFinal  = if hayDanio then perderVida jugadorMovido else jugadorMovido

-- Aplica una transformación a la velocidad del jugador (auxiliar interna).
fijarVel :: (Vector2D -> Vector2D) -> Mundo -> Mundo
fijarVel f m = m { jugadorM = conVelocidad (f (velJugador j)) j }
    where j = jugadorM m

-- manejarEvento
-- Reacciona al teclado: flechas fijan/anulan la velocidad, 'r' reinicia el nivel.
manejarEvento :: Event -> Mundo -> Mundo
manejarEvento (EventKey (SpecialKey KeyLeft)  Down _ _) m = fijarVel (\(_, vy) -> (-100, vy)) m
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m = fijarVel (\(_, vy) -> (100, vy)) m
manejarEvento (EventKey (SpecialKey KeyUp)    Down _ _) m = fijarVel (\(vx, _) -> (vx, 100)) m
manejarEvento (EventKey (SpecialKey KeyDown)  Down _ _) m = fijarVel (\(vx, _) -> (vx, -100)) m
manejarEvento (EventKey (SpecialKey KeyLeft)  Up   _ _) m = fijarVel (\(_, vy) -> (0, vy)) m
manejarEvento (EventKey (SpecialKey KeyRight) Up   _ _) m = fijarVel (\(_, vy) -> (0, vy)) m
manejarEvento (EventKey (SpecialKey KeyUp)    Up   _ _) m = fijarVel (\(vx, _) -> (vx, 0)) m
manejarEvento (EventKey (SpecialKey KeyDown)  Up   _ _) m = fijarVel (\(vx, _) -> (vx, 0)) m
manejarEvento (EventKey (Char 'r') Down _ _)            m = mundoInicial (nivelM m)
manejarEvento _                                                           m = m

