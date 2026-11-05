module Partida
  ( Partida(..)
  , Entrada(..)
  , serializarPartida
  , parsearPartida
  , parsearEntrada
  , parsearLeaderboard
  , mejoresPuntuaciones
  ) where

import Text.Read (readMaybe)
import Data.List (sortOn)
import qualified Data.Ord as Ord
import Tipos
import Nivel (buscarClave)

-- BLOQUE 2 
-- Subtarea 4: 

data Partida = Partida
    { nivelGuardado      :: Int
    , vidasGuardadas     :: Int
    , puntuacionGuardada :: Int
    } deriving (Eq, Show)

data Entrada = Entrada 
    { nombreJugador :: String
    , puntos        :: Int 
    } deriving (Eq, Show)

-- serializarPartida
serializarPartida :: Partida -> [String]
serializarPartida p = 
    [ "NIVEL: " ++ show (nivelGuardado p)
    , "VIDAS: " ++ show (vidasGuardadas p)
    , "PUNTUACION: " ++ show (puntuacionGuardada p)
    ]

-- parsearPartida
parsearPartida :: [String] -> Maybe Partida
parsearPartida lineas = do
    strNivel <- buscarClave "NIVEL: " lineas
    strVidas <- buscarClave "VIDAS: " lineas
    strPuntos <- buscarClave "PUNTUACION: " lineas
    n <- readMaybe strNivel
    v <- readMaybe strVidas
    pt <- readMaybe strPuntos
    return (Partida n v pt)

-- parsearEntrada
parsearEntrada :: String -> Maybe Entrada
parsearEntrada s = 
    case Nivel.splitOn ';' s of
        [nombre, strPts] -> 
            if null nombre 
                then Nothing 
                else do
                    p <- readMaybe strPts
                    return (Entrada nombre p)
        _ -> Nothing

-- parsearLeaderboard
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard = mapM parsearLineaAux
  where
    parsearLineaAux linea
        | all (`elem` " \t\r\n") linea = Nothing
        | otherwise = parsearEntrada linea

-- mejoresPuntuaciones
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n xs = take n (sortOn (Ord.Down . puntos) xs)
