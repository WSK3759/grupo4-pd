module Mundo ( Mundo(..) , 
               mundoInicial , 
               actualizarMundo , 
               manejarEvento
            ) where 

import Tipos 
import Objeto 
import Jugador -- Se usan solo las funciones exportadas, no el constructor 
import Nivel 
import Graphics.Gloss.Interface.Pure.Game

-- 6C. El mundo y su paso de simulación

data Mundo = Mundo
  { jugadorM     :: Jugador
  , enemigosM    :: [Enemigo]
  , plataformasM :: [Plataforma]
  , metasM       :: [Meta]
  , estadoM      :: EstadoJuego
  , nivelM       :: NivelData
  } deriving (Eq, Show)

mundoInicial :: NivelData -> Mundo
mundoInicial nd = Mundo
  { jugadorM     = crearJugador (60, 120)
  , enemigosM    = enemigosDesdeGrid g
  , plataformasM = gridAPlataformas g
  , metasM       = metasDesdeGrid g
  , estadoM      = Jugando
  , nivelM       = nd
  }
  where g = gridNivel nd

actualizarMundo :: Float -> Mundo -> Mundo
actualizarMundo dt mundo 
    | estadoM mundo /= Jugando = mundo
    | otherwise = mundo {
        jugadorM = jugadorActual,
        enemigosM = enemigosAnimados,
        estadoM = nuevoEstado
    }
    where
        -- 1. Animar la lista de enemigos
        enemigosAnimados = map (animar dt) (enemigosM mundo)

        -- 2. Calcular el movimiento del jugador
        jugadorMovido = moverJugador dt (jugadorM mundo)

        -- 3. comprobar colisiones con enemigos y metas
        dano = colisionaConAlguno jugadorMovido enemigosAnimados
        tocaMeta = colisionaConAlguno jugadorMovido (metasM mundo)

        -- 4. Si recibio daño, restar una vida
        jugadorActual = if dano then perderVida jugadorMovido else jugadorMovido

        -- 5. calcular el nuevo estado del juego
        nuevoEstado = estadoSiguiente jugadorActual dano tocaMeta


manejarEvento :: Event -> Mundo -> Mundo
-- Pulsar flechas (Down): Fijan la componente correspondiente 
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m = m { jugadorM = conVelocidad (100, vy) (jugadorM m) } 
    where (_, vy) = velocidadJ (jugadorM m) 

manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m = m { jugadorM = conVelocidad (-100, vy) (jugadorM m) } 
    where (_, vy) = velocidadJ (jugadorM m) 

manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) m = m { jugadorM = conVelocidad (vx, 100) (jugadorM m) } 
    where (vx, _) = velocidadJ (jugadorM m) 

manejarEvento (EventKey (SpecialKey KeyDown) Down _ _) m = m { jugadorM = conVelocidad (vx, -100) (jugadorM m) } 
    where (vx, _) = velocidadJ (jugadorM m) 

-- Soltar flechas (Up): Anulan su componente correspondiente 
manejarEvento (EventKey (SpecialKey KeyRight) Up _ _) m = m { jugadorM = conVelocidad (0, vy) (jugadorM m) } 
    where (_, vy) = velocidadJ (jugadorM m) 

manejarEvento (EventKey (SpecialKey KeyLeft) Up _ _) m = m { jugadorM = conVelocidad (0, vy) (jugadorM m) } 
    where (_, vy) = velocidadJ (jugadorM m) 

manejarEvento (EventKey (SpecialKey KeyUp) Up _ _) m = m { jugadorM = conVelocidad (vx, 0) (jugadorM m) } 
    where (vx, _) = velocidadJ (jugadorM m) 

manejarEvento (EventKey (SpecialKey KeyDown) Up _ _) m = m { jugadorM = conVelocidad (vx, 0) (jugadorM m) } 
    where (vx, _) = velocidadJ (jugadorM m) 

-- Reiniciar nivel con la tecla 'r' o 'R' 
manejarEvento (EventKey (Char 'r') Down _ _) m = mundoInicial (nivelM m) 

manejarEvento (EventKey (Char 'R') Down _ _) m = mundoInicial (nivelM m) 

-- Caso comodín: Cualquier otro evento deja el mundo igual 
manejarEvento _ m = m