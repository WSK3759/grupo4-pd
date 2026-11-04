module Render ( dibujarObjeto , 
                dibujarHUD , 
                mensajeEstado , 
                dibujarMundo 
              ) where 

import Tipos 
import Objeto 
import Jugador 
import Mundo 
import Graphics.Gloss

-- 6B. Del mundo a la pantalla
dibujarObjeto :: Objeto a => Color -> a -> Picture -- Dado un Color y un objeto a, lo transforma en un rectángulo sólido centrado en el centro geométrico de la caja del objeto
dibujarObjeto col obj = Translate (x + w/2) (y + h/2) (Color col (rectangleSolid w h))
    where
        (x, y, w, h) = boundingBox obj

dibujarHUD :: Jugador -> Picture -- Dado un jugador, genera el texto fijo con tres espacios entre Vidas y Puntos, aplicando la escala y traslación indicadas
dibujarHUD j = Translate (-380) 270 $ Scale 0.15 0.15 $ Text ("Vidas: " ++ show (vidas j) ++ "  Puntos: "++ show (puntuacion j))

mensajeEstado :: EstadoJuego -> Maybe String -- Dado un estado del juego, devuelve un Maybe indicando un mensaje del estado en el que se encuentra el juego
mensajeEstado Jugando = Nothing
mensajeEstado PierdeVida = Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver = Just "GAME OVER"
mensajeEstado NivelCompletado = Just "NIVEL COMPLETADO"

dibujarMundo  :: Mundo -> Picture -- compone la escena con Pictures: todo lo del mundo para que la camara siga al jugador, HUD fijo, etc
dibujarMundo mundo = Pictures (escenaDesplazada : hudYMensaje)
    where
        (px, py) = posicionJ (jugadorM mundo)

        -- 1. la escena sigue al jugador
        escena = Pictures [ 
            Pictures [dibujarObjeto (greyN 0.5) p | p <- plataformasM mundo],
            Pictures [dibujarObjeto green metaObj | metaObj <- metasM mundo],
            Pictures [dibujarObjeto red e         | e <- enemigosM mundo],
            dibujarObjeto blue (jugadorM mundo)
            ]
        escenaDesplazada = Translate (realToFrac (-px)) (realToFrac (-py)) escena

        -- 2. HUD e interfaz fija
        hud = dibujarHUD (jugadorM mundo)
        hudYMensaje = case mensajeEstado (estadoM mundo) of
            Nothing -> [hud]
            Just msg -> [hud, Translate (-250) 0 $ Scale 0.3 0.3 $ Color black $ Text msg]