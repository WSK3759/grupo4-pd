module Main (main) where

import Graphics.Gloss (Display(InWindow), white, scale, text, display, play)
import Nivel (parsearNivelCompleto)
import Mundo (mundoInicial, manejarEvento, actualizarMundo)
import Render (dibujarMundo)

nivel1Lineas :: [String]
nivel1Lineas =
  [ "NOMBRE: Colinas"
  , "FONDOS: 1,2,3"
  , "---"
  , "..................M"
  , "..........A........"
  , "####.....####.#####"
  ]

ventana :: Display
ventana = InWindow "Plataformas" (800, 600) (100, 100)

main :: IO ()
main = case parsearNivelCompleto nivel1Lineas of
  Left err -> display ventana white (scale 0.2 0.2 (text (show err)))
  Right nd -> play ventana white 60 (mundoInicial nd) dibujarMundo manejarEvento actualizarMundo