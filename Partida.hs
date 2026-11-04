
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

data Entrada = Entrada { nombreJugador :: String, puntos :: Int }
  deriving (Eq, Show)


-- SUBTAREA 4 - La monada Maybe: partidas guardadas

-- serializarPartida
-- Convierte los datos de una partida en una lista de cadenas
serializarPartida :: Partida -> [String]
serializarPartida p =
    [ "NIVEL: " ++ show (nivelGuardado p) -- usamos show para poder convertir el núm en una cadena
    , "VIDAS: " ++ show (vidasGuardadas p)
    , "PUNTUACION: " ++ show (puntuacionGuardada p)
    ]


-- parsearPartida
-- Convierte una lista de cadenas en una partida
-- Utilizamos Maybe porque puede faltar algun dato o ser inválido
parsearPartida :: [String] -> Maybe Partida
parsearPartida lineas = do
    -- Buscamos los valores de cada campo y obtenemos su texto
    nivelTexto <- buscarClave "NIVEL: " lineas
    vidasTexto <- buscarClave "VIDAS: " lineas
    puntosTexto <- buscarClave "PUNTUACION: " lineas

    -- Convertimos las cadenas a numeros usando readMaybe
    nivel <- readMaybe nivelTexto
    vidas <- readMaybe vidasTexto
    puntuacionLeida <- readMaybe puntosTexto

    -- Construimos la partida con los valores obtenidos
    return (Partida nivel vidas puntuacionLeida)

-- Si alguna busqueda o conversion devuelve Nothing, toda la funcion devuelve Nothing


-- SUBTAREA 4 - La monada Maybe

-- parsearEntrada
-- Convierte una cadena "nombre;puntos" en una Entrada.
-- Si el formato es incorrecto devuelve Nothing.
parsearEntrada :: String -> Maybe Entrada
parsearEntrada linea =
    case splitOn ';' linea of
        [nombre, puntosTexto] | not (null nombre) -> do
            puntuacionLeida <- readMaybe puntosTexto
            return (Entrada nombre puntuacionLeida)
        _ -> Nothing

-- BLOQUE 2 - SUBTAREA 4 - La monada Maybe

-- parsearLeaderboard
-- Convierte una lista de cadenas en una lista de entradas.
-- Ignora las lineas vacias y devuelve Nothing si alguna es invalida.
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard lineas =
    mapM parsearEntrada (filter (not . null) lineas)


-- SUBTAREA 4 - La monada Maybe

-- mejoresPuntuaciones
-- Devuelve las n mejores entradas ordenadas de mayor a menor puntuacion.
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n entradas =
    take n -- se queda con los n primeros elementos de la lista ya ordenada (4)
    (sortOn -- ordenar según la puntuación (3)
        (Down . puntos) -- combinar dos operaciones mediante composición... puntos devuelve la puntuación de un jugador
                entradas)


------------------------------------------------
-- BONUS - Functor, Applicative y Monad
------------------------------------------------

-- BONUS 1 - parsearPartidaA (0.25 puntos)
-- Convierte las lineas de una partida en un valor Maybe Partida.
-- Utilizamos Applicative para construir la partida.
parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA lineas =
    Partida
        <$> (buscarClave "NIVEL: " lineas >>= readMaybe)
        <*> (buscarClave "VIDAS: " lineas >>= readMaybe)
        <*> (buscarClave "PUNTUACION: " lineas >>= readMaybe)


-- BONUS 2 - puntuacionTotal (0.25 puntos)
-- Calcula la suma de puntos de una tabla de puntuaciones valida.
-- Utilizamos Functor para aplicar la suma dentro de Maybe.
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal lineas =
    sum . map puntos <$> parsearLeaderboard lineas
