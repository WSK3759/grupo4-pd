module Partida
    ( Partida(..)
    , Entrada(..)
    , serializarPartida
    , parsearPartida
    , parsearEntrada
    , parsearLeaderboard
    , mejoresPuntuaciones
    ) where

import Nivel (buscarClave, splitOn)
import Text.Read (readMaybe)
import Data.List (sortOn)
import Data.Ord (Down(..))
data Partida = Partida
    { nivelGuardado      :: Int
    , vidasGuardadas     :: Int
    , puntuacionGuardada :: Int
    } deriving (Eq, Show)

data Entrada = Entrada
    { nombreJugador :: String
    , puntos :: Int
    } deriving (Eq, Show)

serializarPartida :: Partida -> [String]
serializarPartida partida =
    [ "NIVEL: " ++ show (nivelGuardado partida)
    , "VIDAS: " ++ show (vidasGuardadas partida)
    , "PUNTUACION: " ++ show (puntuacionGuardada partida)
    ]

--import Text.Read (readMaybe)
parsearPartida :: [String] -> Maybe Partida
parsearPartida lineas = do
    nivelE <- buscarClave "NIVEL: " lineas
    vidasE <- buscarClave "VIDAS: " lineas
    puntuacionE <- buscarClave "PUNTUACION: " lineas

    nivel <- readMaybe nivelE
    vidas <- readMaybe vidasE
    puntuacion <- readMaybe puntuacionE

    return (Partida nivel vidas puntuacion)

parsearEntrada :: String -> Maybe Entrada
parsearEntrada linea 
    | length valores /= 2 = Nothing
    | null nombre        = Nothing
    | otherwise          = do
        puntuacion <- readMaybe puntuacionE
        return (Entrada nombre puntuacion)
    where
        valores = splitOn ';' linea
        nombre = head valores
        puntuacionE = last valores


parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard lineas =
    mapM parsearEntrada (filter (not . null) lineas)

-- Calcula la puntuación total de una tabla de puntuaciones válida.
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal lineas =
    sum . map puntos <$> parsearLeaderboard lineas

--Obtener las n mejores entradas, ordenadas de mayor a menor gracias a Down
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n entradas =
    take n (sortOn (Down . puntos) entradas)

parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA lineas =
    Partida <$> (buscarClave "NIVEL: " lineas >>= readMaybe)
            <*> (buscarClave "VIDAS: " lineas >>= readMaybe)
            <*> (buscarClave "PUNTUACION: " lineas >>= readMaybe)
