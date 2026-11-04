module Partida ( Partida(..) , 
                 Entrada(..) , 
                 serializarPartida , 
                 parsearPartida , 
                 parsearEntrada , 
                 parsearLeaderboard , 
                 mejoresPuntuaciones,
                 parsearPartidaA, 
                 puntuacionTotal
               ) where 

import Nivel (splitOn, buscarClave) 
import Text.Read (readMaybe) 
import Data.List (sortOn) 
import qualified Data.Ord as Ord -- Importación cualificada explícita

-- BLOQUE 2 - Mónadas 
-- SUBTAREA 4 — La mónada Maybe: leer una partida guardada y una tabla de puntuaciones

data Partida = Partida
  { nivelGuardado      :: Int
  , vidasGuardadas     :: Int
  , puntuacionGuardada :: Int
  } deriving (Eq, Show)

data Entrada = Entrada { nombreJugador :: String, puntos :: Int }
  deriving (Eq, Show)

serializarPartida :: Partida -> [String] -- Dada un tipo Partida, serializa la partida transformandolo en una [String] con formato predefinido
serializarPartida p = ["NIVEL: " ++ show (nivelGuardado p), "VIDAS: "++ show (vidasGuardadas p), "PUNTUACION: " ++ show (puntuacionGuardada p)]

parsearPartida :: [String] -> Maybe Partida -- Dada un fichero, devuelve su parseo al tipo Partida
parsearPartida [] = Nothing
parsearPartida lineas = do
    nivel <- buscarClave "NIVEL: " lineas
    vidas <- buscarClave "VIDAS: " lineas
    puntos <- buscarClave "PUNTUACION: " lineas
    n <- readMaybe nivel
    v <- readMaybe vidas
    p <- readMaybe puntos
    return (Partida n v p)

parsearEntrada :: String -> Maybe Entrada -- Dada una entrada para un jugador en formato String, lo parsea al tipo Entrada 
parsearEntrada entrada = case splitOn ';' entrada of
    [nombre, puntos] | not (null nombre) -> do
        p <- readMaybe puntos
        return (Entrada nombre p)
    _ -> Nothing

parsearLeaderboard :: [String] -> Maybe [Entrada] -- Dada una serie de entradas en formato String, devuelve su parseo en formato lista de tipo Entrada
parsearLeaderboard entradas = mapM parsearEntrada entradasLimpias
    where
        entradasLimpias = filter (not . null) entradas

mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada] -- Dado un Int n y una lista de Entradas, devuelve las n Entradas con mayor puntuacion
mejoresPuntuaciones n entradasParseadas = take n (sortOn (Ord.Down . puntos) entradasParseadas)


-- BONUS
parsearPartidaA :: [String] -> Maybe Partida          -- módulo Partida
parsearPartidaA lineas = 
    Partida 
        <$> (buscarClave "NIVEL: " lineas >>= readMaybe) 
        <*> (buscarClave "VIDAS: " lineas >>= readMaybe) 
        <*> (buscarClave "PUNTUACION: " lineas >>= readMaybe)

puntuacionTotal :: [String] -> Maybe Int 
puntuacionTotal lineas = sum . map puntos <$> parsearLeaderboard lineas
