module Render (dibujarObjeto, dibujarHUD, mensajeEstado, dibujarMundo) where

import Graphics.Gloss
import Jugador (Jugador, posJugador, puntuacion, vidas)
import Mundo (Mundo(..))
import Objeto (Objeto(..))
import Tipos (EstadoJuego(..))

-- Dibuja cualquier objeto como un rectángulo de color con el tamaño y posición de su caja.
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col o = translate (x + w / 2) (y + h / 2) (color col (rectangleSolid w h))
  where (x, y, w, h) = boundingBox o

-- Dibuja el texto con las vidas y los puntos del jugador.
dibujarHUD :: Jugador -> Picture
dibujarHUD j = translate (-380) 270 (scale 0.15 0.15 (text texto))
  where texto = "Vidas: " ++ show (vidas j) ++ "   Puntos: " ++ show (puntuacion j)

-- Mensaje a mostrar en pantalla según el estado (Nothing mientras se juega).
mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando         = Nothing
mensajeEstado PierdeVida      = Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver        = Just "GAME OVER"
mensajeEstado NivelCompletado = Just "NIVEL COMPLETADO"

-- Dibuja el mundo: escena con cámara siguiendo al jugador, HUD fijo y mensaje de estado.
dibujarMundo :: Mundo -> Picture
dibujarMundo m = pictures ([escena, dibujarHUD jugador] ++ maybe [] (\msg -> [cartel msg]) (mensajeEstado (estadoM m)))
  where
    jugador  = jugadorM m
    (px, py) = posJugador jugador
    escena   = translate (-px) (-py) (pictures
                 (  map (dibujarObjeto (greyN 0.5)) (plataformasM m)
                 ++ map (dibujarObjeto green)       (metasM m)
                 ++ map (dibujarObjeto red)         (enemigosM m)
                 ++ [dibujarObjeto blue jugador] ))
    cartel msg = translate (-250) 0 (scale 0.3 0.3 (color black (text msg)))
