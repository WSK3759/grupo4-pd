module Render
  ( dibujarMundo
  ) where

import Graphics.Gloss
import Tipos
import Objeto
import Jugador
import Mundo

-- dibujarObjeto
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col o = 
    let (x, y, w, h) = boundingBox o
        centroX = x + w / 2
        centroY = y + h / 2
    in Translate centroX centroY (Color col (rectangleSolid w h))

-- dibujarHUD
dibujarHUD :: Jugador -> Picture
dibujarHUD j = 
    Translate (-380) 270 (Scale 0.15 0.15 (Text ("Vidas: " ++ show (vidas j) ++ "   Puntos: " ++ show (puntuacion j))))

-- mensajeEstado
mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando = Nothing
mensajeEstado PierdeVida = Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver = Just "GAME OVER"
mensajeEstado NivelCompletado = Just "NIVEL COMPLETADO"

-- dibujarMundo
dibujarMundo :: Mundo -> Picture
dibujarMundo m = 
    let (px, py) = posJugador (jugadorM m)
        escena = Translate (-px) (-py) (Pictures 
            [ Pictures (map (dibujarObjeto (greyN 0.5)) (plataformasM m))
            , Pictures (map (dibujarObjeto green) (metasM m))
            , Pictures (map (dibujarObjeto red) (enemigosM m))
            , dibujarObjeto blue (jugadorM m)
            ])
        hud = dibujarHUD (jugadorM m)
        mensajePic = case mensajeEstado (estadoM m) of
            Nothing -> Blank
            Just msg -> Translate (-250) 0 (Scale 0.3 0.3 (Color black (Text msg)))
    in Pictures [escena, hud, mensajePic]
