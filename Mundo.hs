module Mundo
    ( Mundo(..)
    , mundoInicial
    , actualizarMundo
    , manejarEvento
    ) where
import Tipos (EstadoJuego(..))
import Objeto (Enemigo, Plataforma, Meta, Objeto(..), colisionaConAlguno)
import Jugador
import Nivel (NivelData, gridNivel, enemigosDesdeGrid, gridAPlataformas, metasDesdeGrid)
import qualified Graphics.Gloss.Interface.Pure.Game as G
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

manejarEvento :: G.Event -> Mundo -> Mundo
manejarEvento evento m =
    case evento of
        G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _ ->
            cambiarVelocidad (100, snd vel)
        G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _ ->
            cambiarVelocidad (-100, snd vel)
        G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _ ->
            cambiarVelocidad (fst vel, 100)
        G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _ ->
            cambiarVelocidad (fst vel, -100)
        G.EventKey (G.SpecialKey G.KeyRight) G.Up _ _ ->
            cambiarVelocidad (0, snd vel)
        G.EventKey (G.SpecialKey G.KeyLeft) G.Up _ _ ->
            cambiarVelocidad (0, snd vel)
        G.EventKey (G.SpecialKey G.KeyUp) G.Up _ _ ->
            cambiarVelocidad (fst vel, 0)
        G.EventKey (G.SpecialKey G.KeyDown) G.Up _ _ ->
            cambiarVelocidad (fst vel, 0)
        G.EventKey (G.Char 'r') G.Down _ _ ->
            mundoInicial (nivelM m)
        _ -> m
    where
        vel = velocidad (jugadorM m)

        cambiarVelocidad nuevaVel =
            m { jugadorM = conVelocidad nuevaVel (jugadorM m) }

actualizarMundo :: Float -> Mundo -> Mundo
actualizarMundo dt m
    | estadoM m /= Jugando = m
    | otherwise =
        let jugadorMovido = moverJugador (realToFrac dt) (jugadorM m)
            enemigosMovidos = map (animar (realToFrac dt)) (enemigosM m)

            recibioDano = colisionaConAlguno jugadorMovido enemigosMovidos
            tocaMeta = colisionaConAlguno jugadorMovido (metasM m)
            jugadorFinal = if recibioDano then perderVida jugadorMovido else jugadorMovido
            nuevoEstado = estadoSiguiente jugadorFinal recibioDano tocaMeta

        in m
            { jugadorM = jugadorFinal
            , enemigosM = enemigosMovidos
            , estadoM = nuevoEstado
            }




