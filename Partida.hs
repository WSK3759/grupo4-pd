
module Partida
    ( Partida(..), Entrada(..)
    , serializarPartida, parsearPartida
    , parsearEntrada, parsearLeaderboard
    , mejoresPuntuaciones
    , parsearPartidaA, puntuacionTotal -- bonus
    ) where

import Nivel (buscarClave, splitOn)
import Text.Read (readMaybe)

import Data.List (sortOn)
import Data.Ord (Down(..))

------------------------------------------------
-- Bloque 2 - Monadas (3.0 puntos)
------------------------------------------------

-- SUBTAREA 4 - La mónada Maybe: leer una partida guardada y una tabla de puntuaciones (1.25 puntos)

-- Representa los datos de una partida guardada (viene definida en enunciado)
data Partida = Partida
    { nivelGuardado       :: Int
    , vidasGuardadas      :: Int
    , puntuacionGuardada  :: Int
    }
    deriving (Eq, Show)

-- Representa una entrada de la tabla de puntuaciones (viene definida en enunciado)
data Entrada = Entrada
    { nombreJugador :: String
    , puntos        :: Int
    }
    deriving (Eq, Show)


-- SUBTAREA 4 - La monada Maybe: partidas guardadas

-- serializarPartida
-- Convierte los datos de una partida en una lista de cadenas
serializarPartida :: Partida -> [String]
serializarPartida partida =
    [ "NIVEL: " ++ show (nivelGuardado partida)
    , "VIDAS: " ++ show (vidasGuardadas partida)
    , "PUNTUACION: " ++ show (puntuacionGuardada partida)
    ]


-- parsearPartida
-- Convierte una lista de cadenas en una partida
-- Utilizamos Maybe porque puede faltar algun dato o ser inválido
parsearPartida :: [String] -> Maybe Partida
parsearPartida lineas = do
    nivelE <- buscarClave "NIVEL: " lineas
    vidasE <- buscarClave "VIDAS: " lineas
    puntuacionE <- buscarClave "PUNTUACION: " lineas

    nivel <- readMaybe nivelE
    vidas <- readMaybe vidasE
    puntuacion <- readMaybe puntuacionE

    return (Partida nivel vidas puntuacion)


-- SUBTAREA 4 - La monada Maybe

-- parsearEntrada
-- Convierte una cadena "nombre;puntos" en una Entrada.
-- Si el formato es incorrecto devuelve Nothing.
parsearEntrada :: String -> Maybe Entrada
parsearEntrada s = case splitOn ';' s of
        [nombre, p] | not (null nombre) -> Entrada nombre <$> readMaybe p
        _        -> Nothing


-- BLOQUE 2 - SUBTAREA 4 - La monada Maybe

-- parsearLeaderboard
-- Indica si una línea está vacía o solo tiene espacios (auxiliar interna).
lineaBlanca :: String -> Bool
lineaBlanca = all (`elem` " \t\r")

-- Lee toda la tabla ignorando líneas en blanco; Nothing si alguna otra línea es inválida.
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard = mapM parsearEntrada . filter (not . lineaBlanca)



-- SUBTAREA 4 - La monada Maybe

-- mejoresPuntuaciones
-- Devuelve las n mejores entradas ordenadas de mayor a menor puntuacion.
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n = take n . sortOn (Down . puntos)

------------------------------------------------
-- BONUS - Functor, Applicative y Monad
------------------------------------------------

-- BONUS 1 - parsearPartidaA (0.25 puntos)
-- Convierte las lineas de una partida en un valor Maybe Partida.
-- igual que parsearPartida pero con <$> y <*> (Applicative).
parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA ls = Partida <$> campo "NIVEL: " <*> campo "VIDAS: " <*> campo "PUNTUACION: "
  where campo clave = buscarClave clave ls >>= readMaybe

-- BONUS 2 - puntuacionTotal (0.25 puntos)
-- Calcula la suma de puntos de una tabla de puntuaciones valida.
-- suma los puntos de una tabla válida usando fmap (Functor).
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal ls = sum . map puntos <$> parsearLeaderboard ls

