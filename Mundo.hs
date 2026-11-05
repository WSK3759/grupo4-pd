module Mundo
  ( Mundo(..)
  , mundoInicial
  , actualizarMundo
  , manejarEvento
  ) where

import Graphics.Gloss.Interface.Pure.Game
import Tipos
import Objeto
import Jugador
import qualified Nivel as N

-- BLOQUE 3 — Subtarea 6B

-- Definición del tipo Mundo 
data Mundo = Mundo
  { jugadorM     :: Jugador
  , enemigosM    :: [Enemigo]
  , plataformasM :: [Plataforma]
  , metasM       :: [Meta]
  , estadoM      :: EstadoJuego
  , nivelM       :: N.NivelData
  } deriving (Eq, Show)

-- Subtarea 6C
mundoInicial :: N.NivelData -> Mundo
mundoInicial nd = Mundo
  { jugadorM     = crearJugador (60, 120)
  , enemigosM    = N.enemigosDesdeGrid g
  , plataformasM = N.gridAPlataformas g
  , metasM       = N.metasDesdeGrid g
  , estadoM      = Jugando
  , nivelM       = nd
  }
  where g = N.gridNivel nd

-- actualizarMundo
actualizarMundo :: Float -> Mundo -> Mundo
actualizarMundo dt m = 
    if estadoM m /= Jugando
        then m
        else 
            -- Mover al jugador usando la función de física/movimiento
            let jMovido = moverJugador dt (jugadorM m)
                jActualizado = animar dt jMovido  -- por si también tiene animación de sprites
                
                -- Animar enemigos
                enemigosAnimados = map (animar dt) (enemigosM m)
                
                -- Comprobar colisiones con enemigos (daño -> pierdeVida)
                tocaEnemigo = any (colisionaCon jActualizado) enemigosAnimados
                jTrasEnemigo = if tocaEnemigo then perderVida jActualizado else jActualizado
                
                -- Comprobar colisiones con metas
                tocaMeta = any (colisionaCon jActualizado) (metasM m)
                
                -- Determinar el estado siguiente del juego
                nuevoEstado = estadoSiguiente jTrasEnemigo tocaEnemigo tocaMeta
            in m { jugadorM     = jTrasEnemigo
                 , enemigosM    = enemigosAnimados
                 , estadoM      = nuevoEstado
                 }

-- manejarEvento con las direcciones corregidas
manejarEvento :: Event -> Mundo -> Mundo
manejarEvento evento m = case evento of
    -- Tecla R para reiniciar
    EventKey (Char 'r') Graphics.Gloss.Interface.Pure.Game.Down _ _ -> mundoInicial (nivelM m)
    
    -- Movimiento horizontal (Invertimos -100 y 100 si iban al revés)
    EventKey (SpecialKey KeyLeft) Graphics.Gloss.Interface.Pure.Game.Down _ _ -> 
        let (_, vy) = velJugador (jugadorM m)
        in m { jugadorM = conVelocidad (100, vy) (jugadorM m) }
    EventKey (SpecialKey KeyLeft) Up _ _ -> 
        let (vx, vy) = velJugador (jugadorM m)
            nuevaVx = if vx > 0 then 0 else vx
        in m { jugadorM = conVelocidad (nuevaVx, vy) (jugadorM m) }
        
    EventKey (SpecialKey KeyRight) Graphics.Gloss.Interface.Pure.Game.Down _ _ -> 
        let (_, vy) = velJugador (jugadorM m)
        in m { jugadorM = conVelocidad (-100, vy) (jugadorM m) }
    EventKey (SpecialKey KeyRight) Up _ _ -> 
        let (vx, vy) = velJugador (jugadorM m)
            nuevaVx = if vx < 0 then 0 else vx
        in m { jugadorM = conVelocidad (nuevaVx, vy) (jugadorM m) }

    -- Movimiento vertical (Invertimos Arriba/Abajo si era necesario)
    EventKey (SpecialKey KeyUp) Graphics.Gloss.Interface.Pure.Game.Down _ _ -> 
        let (vx, _) = velJugador (jugadorM m)
        in m { jugadorM = conVelocidad (vx, -100) (jugadorM m) }
    EventKey (SpecialKey KeyUp) Up _ _ -> 
        let (vx, vy) = velJugador (jugadorM m)
            nuevaVy = if vy < 0 then 0 else vy
        in m { jugadorM = conVelocidad (vx, nuevaVy) (jugadorM m) }

    EventKey (SpecialKey KeyDown) Graphics.Gloss.Interface.Pure.Game.Down _ _ -> 
        let (vx, _) = velJugador (jugadorM m)
        in m { jugadorM = conVelocidad (vx, 100) (jugadorM m) }
    EventKey (SpecialKey KeyDown) Up _ _ -> 
        let (vx, vy) = velJugador (jugadorM m)
            nuevaVy = if vy > 0 then 0 else vy
        in m { jugadorM = conVelocidad (vx, nuevaVy) (jugadorM m) }

    -- Cualquier otro evento deja el mundo intacto
    _ -> m
