module Nivel
  ( ErrorNivel(..), NivelData(..)
  , splitOn, buscarClave
  , parsearFichero, validarNivel, parsearNivelCompleto
  , tamCelda, posicionCelda, gridAPlataformas, metasDesdeGrid, enemigosDesdeGrid
  , parsearNiveles, todasLasPosiciones
  ) where

import Control.Monad (msum)
import Data.List (stripPrefix)
import Objeto (Enemigo(..), Meta(..), Plataforma(..))
import Text.Read (readMaybe)
import Tipos (Celda(..), Grid, TipoEnemigo(..), Vector2D, esMeta, parsearGrid)

-- Por qué falla la lectura o validación de un nivel.
data ErrorNivel
  = FormatoInvalido String
  | NivelSinMeta
  | NivelConVariasMetas Int
  deriving (Eq, Show)

-- Un nivel ya interpretado: nombre, fondos y mapa.
data NivelData = NivelData
  { nombreNivel :: String
  , fondos      :: [Int]
  , gridNivel   :: Grid
  } deriving (Eq, Show)

-- Divide una cadena por un carácter separador.
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])  -> [a]
  (a, _:b) -> a : splitOn c b

-- Devuelve lo que sigue a la clave en la primera línea que empiece por ella.
buscarClave :: String -> [String] -> Maybe String
buscarClave clave = msum . map (stripPrefix clave)

-- Convierte un Maybe en Either usando el error dado si es Nothing (auxiliar interna).
maybeAEither :: e -> Maybe a -> Either e a
maybeAEither e = maybe (Left e) Right

-- Como buscarClave, pero con un FormatoInvalido si la clave falta (auxiliar interna).
buscarOError :: String -> [String] -> Either ErrorNivel String
buscarOError clave ls =
  maybeAEither (FormatoInvalido ("falta la clave '" ++ clave ++ "'")) (buscarClave clave ls)

-- Separa cabecera y mapa en la línea '---'; falla si falta el separador o el mapa (auxiliar interna).
separarCabecera :: [String] -> Either ErrorNivel ([String], [String])
separarCabecera ls = case break (== "---") ls of
  (_, [])         -> Left (FormatoInvalido "falta el separador '---'")
  (cabecera, _:r) -> case filter (not . null) r of
    []   -> Left (FormatoInvalido "no hay mapa tras '---'")
    mapa -> Right (cabecera, mapa)

-- Interpreta el texto de un fichero de nivel (cabecera, '---' y mapa).
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero ls = do
  (cabecera, mapa) <- separarCabecera ls
  nombre           <- buscarOError "NOMBRE: " cabecera
  textoFondos      <- buscarOError "FONDOS: " cabecera
  fs <- maybeAEither (FormatoInvalido "FONDOS contiene valores no numericos")
                     (mapM readMaybe (splitOn ',' textoFondos))
  return (NivelData nombre fs (parsearGrid mapa))

-- Comprueba que el nivel tenga exactamente una meta.
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nd = case length (filter esMeta (concat (gridNivel nd))) of
  0 -> Left NivelSinMeta
  1 -> Right nd
  n -> Left (NivelConVariasMetas n)

-- Interpreta y valida un nivel: parsearFichero seguido de validarNivel.
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto ls = parsearFichero ls >>= validarNivel

-- Lado de cada celda del mapa en unidades del mundo.
tamCelda :: Float
tamCelda = 40

-- Esquina inferior izquierda de la celda (fila i desde arriba, columna j) en un mapa de total filas.
posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j = (fromIntegral j * tamCelda, fromIntegral (total - 1 - i) * tamCelda)

-- Una plataforma por cada celda sólida del mapa.
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas g =
  [ Plataforma (x, y, tamCelda, tamCelda)
  | (i, fila) <- zip [0 ..] g
  , (j, Solido) <- zip [0 ..] fila
  , let (x, y) = posicionCelda (length g) i j
  ]

-- Una meta por cada celda META del mapa.
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid g =
  [ Meta (posicionCelda (length g) i j)
  | (i, fila) <- zip [0 ..] g
  , (j, META) <- zip [0 ..] fila
  ]

-- Un enemigo patrulla, centrado en su celda, por cada InicioEnemigo del mapa.
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid g =
  [ Enemigo ident (x + tamCelda / 2, y + tamCelda / 2) (-40, 0) (tamCelda, tamCelda) Patrulla
  | (i, fila) <- zip [0 ..] g
  , (j, InicioEnemigo ident) <- zip [0 ..] fila
  , let (x, y) = posicionCelda (length g) i j
  ]

-- BONUS: interpreta y valida varios niveles; devuelve todos o el primer error.
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles = traverse parsearNivelCompleto

-- BONUS: todas las posiciones (fila, columna) de una rejilla rectangular, por filas.
todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones g = (,) <$> [0 .. length g - 1] <*> [0 .. columnas - 1]
  where columnas = length (concat (take 1 g))
