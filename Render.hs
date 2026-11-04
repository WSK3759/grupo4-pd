
module Render
    ( dibujarObjeto, dibujarHUD
    , mensajeEstado, dibujarMundo
    ) where


import Graphics.Gloss
import Tipos
import Objeto
import qualified Jugador as J -- importa el módulo Jugador, pero para utilizar sus elementos escribe J
import Mundo

------------------------------------------------
-- Bloque 3 - Gloss: del mapa al mundo jugable
------------------------------------------------

-- SUBTAREA 6B - Del mundo a la pantalla (0.75 puntos)

-- dibujarObjeto
-- Dibuja un objeto como un rectangulo del color indicado.
-- Utiliza su caja de colision para obtener la posicion y el tamaño.
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col objeto =
    translate (x + w/2) (y + h/2)
        (color col (rectangleSolid w h))
    where
        (x,y,w,h) = boundingBox objeto


-- dibujarHUD
-- Muestra las vidas y los puntos del jugador en pantalla.
-- dibujarHUD :: Jugador -> Picture
-- dibujarHUD j =
--     translate (-380) 270
--         (scale 0.15 0.15
--             (text ("Vidas: " ++ show (vidas j) ++
--                    "   Puntos: " ++ show (puntuacion j))))

-- dibujarHUD
-- Utilizamos una importacion cualificada de Jugador (J).
dibujarHUD :: J.Jugador -> Picture
dibujarHUD j =
    translate (-380) 270
        (scale 0.15 0.15
            (text ("Vidas: " ++ show (J.vidas j)
                ++ "   Puntos: " ++ show (J.puntuacion j))))


-- mensajeEstado
-- Devuelve el mensaje correspondiente al estado del juego.
-- Si estamos jugando, no hay ningun mensaje.
mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando = Nothing
mensajeEstado PierdeVida = Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver = Just "GAME OVER"
mensajeEstado NivelCompletado = Just "NIVEL COMPLETADO"


-- dibujarMundo
-- Dibuja todos los elementos del mundo, el HUD y el mensaje de estado.
-- La camara se desplaza para seguir la posicion del jugador.
dibujarMundo :: Mundo -> Picture
dibujarMundo mundo = pictures
    [
      -- Escena del juego desplazada con la posicion del jugador.
      translate (-px) (-py) (pictures
        [ pictures (map (dibujarObjeto (greyN 0.5)) (plataformasM mundo))
        , pictures (map (dibujarObjeto green) (metasM mundo))
        , pictures (map (dibujarObjeto red) (enemigosM mundo))
        , dibujarObjeto blue (jugadorM mundo)
        ])

      -- El HUD permanece fijo en la pantalla.
    , dibujarHUD (jugadorM mundo)

      -- Mostramos un mensaje solo si el estado lo necesita.
    , case mensajeEstado (estadoM mundo) of
        Nothing -> Blank
        Just msg -> translate (-250) 0
                        (scale 0.3 0.3 (color black (text msg)))
    ]
    where
        (px,py) = posicion (jugadorM mundo)
