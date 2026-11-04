module Partida
  ( Partida(..), Entrada(..)
  , serializarPartida, parsearPartida, parsearEntrada
  , parsearLeaderboard, mejoresPuntuaciones
  , parsearPartidaA, puntuacionTotal
  ) where

-- Importación cualificada: Data.List exporta muchos nombres comunes (lookup, insert...),
-- así que se usa con prefijo L. y se ve de dónde viene cada función.
import qualified Data.List as L
import Data.Ord (Down(..))
import Nivel (buscarClave, splitOn)
import Text.Read (readMaybe)

-- Datos de una partida guardada.
data Partida = Partida
  { nivelGuardado      :: Int
  , vidasGuardadas     :: Int
  , puntuacionGuardada :: Int
  } deriving (Eq, Show)

-- Una línea de la tabla de puntuaciones.
data Entrada = Entrada { nombreJugador :: String, puntos :: Int }
  deriving (Eq, Show)

-- Convierte una partida en las líneas del fichero de guardado.
serializarPartida :: Partida -> [String]
serializarPartida (Partida n v p) =
  ["NIVEL: " ++ show n, "VIDAS: " ++ show v, "PUNTUACION: " ++ show p]

-- Lee una partida guardada; Nothing si falta una clave o un número es ilegible.
parsearPartida :: [String] -> Maybe Partida
parsearPartida ls = do
  n <- buscarClave "NIVEL: " ls >>= readMaybe
  v <- buscarClave "VIDAS: " ls >>= readMaybe
  p <- buscarClave "PUNTUACION: " ls >>= readMaybe
  return (Partida n v p)

-- Lee una línea "nombre;puntos" con nombre no vacío y exactamente dos campos.
parsearEntrada :: String -> Maybe Entrada
parsearEntrada s = case splitOn ';' s of
  [nombre, p] | not (null nombre) -> Entrada nombre <$> readMaybe p
  _                               -> Nothing

-- Indica si una línea está vacía o solo tiene espacios (auxiliar interna).
lineaBlanca :: String -> Bool
lineaBlanca = all (`elem` " \t\r")

-- Lee toda la tabla ignorando líneas en blanco; Nothing si alguna otra línea es inválida.
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard = mapM parsearEntrada . filter (not . lineaBlanca)

-- Las n mejores entradas, de mayor a menor puntuación.
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n = take n . L.sortOn (Down . puntos)

-- BONUS: igual que parsearPartida pero con <$> y <*> (Applicative).
parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA ls = Partida <$> campo "NIVEL: " <*> campo "VIDAS: " <*> campo "PUNTUACION: "
  where campo clave = buscarClave clave ls >>= readMaybe

-- BONUS: suma los puntos de una tabla válida usando fmap (Functor).
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal ls = sum . map puntos <$> parsearLeaderboard ls
