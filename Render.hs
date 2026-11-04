module Render
    ( dibujarObjeto
    , dibujarHUD
    , mensajeEstado
    , dibujarMundo
    ) where

import Tipos (EstadoJuego(..))
import Objeto (Objeto(..))
import Jugador (Jugador, posicionJ, vidas, puntuacion)
import Mundo (Mundo, jugadorM, plataformasM, metasM, enemigosM, estadoM)
import Graphics.Gloss
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col objeto =
    let (x, y, ancho, alto) = boundingBox objeto
    in translate (realToFrac (x + ancho / 2))
                 (realToFrac (y + alto / 2))
                 (color col (rectangleSolid (realToFrac ancho)
                                            (realToFrac alto)))

dibujarHUD :: Jugador -> Picture
dibujarHUD j =
    translate (-380) 270
        (scale 0.15 0.15
            (text ("Vidas: " ++ show (vidas j) ++
                   "   Puntos: " ++ show (puntuacion j))))


mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado estado =
    case estado of
        Jugando         -> Nothing
        PierdeVida      -> Just "PIERDE VIDA (R para reiniciar)"
        GameOver        -> Just "GAME OVER"
        NivelCompletado -> Just "NIVEL COMPLETADO"


dibujarMundo :: Mundo -> Picture
dibujarMundo m =
    Pictures
        [ translate (realToFrac (-px)) (realToFrac (-py)) (Pictures escena)
        , dibujarHUD (jugadorM m)
        , case mensajeEstado (estadoM m) of
            Nothing -> Blank
            Just msg ->
                translate (-250) 0
                    (scale 0.3 0.3
                        (color black (text msg)))
        ]
    where
        (px, py) = posicionJ (jugadorM m)

        escena =
            map (dibujarObjeto (greyN 0.5)) (plataformasM m)
            ++ map (dibujarObjeto green) (metasM m)
            ++ map (dibujarObjeto red) (enemigosM m)
            ++ [dibujarObjeto blue (jugadorM m)]
