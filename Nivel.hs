module Nivel
  ( ErrorNivel(..)
  , NivelData(..)
  , splitOn
  , buscarClave
  , parsearFichero
  , validarNivel
  , parsearNivelCompleto
  , tamCelda
  , posicionCelda
  , gridAPlataformas
  , metasDesdeGrid
  , enemigosDesdeGrid
  ) where

import Text.Read (readMaybe)
import Data.List (stripPrefix)
import Tipos
import Objeto

-- Subtarea 5

data ErrorNivel
    = FormatoInvalido String
    | NivelSinMeta
    | NivelConVariasMetas Int
    deriving (Eq, Show)

data NivelData = NivelData
    { nombreNivel :: String
    , fondos      :: [Int]
    , gridNivel   :: Grid
    } deriving (Eq, Show)

-- splitOn
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
    (a, [])  -> [a]
    (a, _:b) -> a : splitOn c b

-- buscarClave
buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (x:xs) = case stripPrefix clave x of
    Just resto -> Just resto
    Nothing    -> buscarClave clave xs

-- parsearFichero
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero lineas = do
    -- 1. Separar las líneas de la cabecera y el mapa usando el separador "---"
    let (cabecera, resto) = span (/= "---") lineas
    if null resto 
        then Left (FormatoInvalido "falta el separador '---'")
        else return ()
    
    let mapaLineas = tail resto -- quitamos el "---"
    if null mapaLineas
        then Left (FormatoInvalido "falta el mapa tras el separador")
        else return ()

    -- 2. Buscar las claves obligatorias en la cabecera usando buscarClave (de Subt. 4)
    strNombre <- case buscarClave "NOMBRE: " cabecera of
        Just n  -> Right n
        Nothing -> Left (FormatoInvalido "falta la clave 'NOMBRE: '")

    strFondos <- case buscarClave "FONDOS: " cabecera of
        Just f  -> Right f
        Nothing -> Left (FormatoInvalido "falta la clave 'FONDOS: '")

    -- 3. Parsear los fondos numéricos separados por comas
    let listaFondosStr = splitOn ',' strFondos
    fondosParsed <- mapM (\s -> case readMaybe (trim s) of
        Just val -> Right val
        Nothing  -> Left (FormatoInvalido "fondo no numérico")
        ) listaFondosStr

    -- 4. Parsear el grid del nivel
    let gridParsed = parsearGrid mapaLineas

    return (NivelData strNombre fondosParsed gridParsed)
  where
    trim = reverse . dropWhile (`elem` " \t\r\n") . reverse . dropWhile (`elem` " \t\r\n")

-- validarNivel
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nivelData = 
    let grid = gridNivel nivelData
        -- Contar cuántas celdas META hay en todo el grid
        metas = [celda | fila <- grid, celda <- fila, esMeta celda]
        cantidadMetas = length metas
    in case cantidadMetas of
        0 -> Left NivelSinMeta
        1 -> Right nivelData
        n -> Left (NivelConVariasMetas n)

-- parsearNivelCompleto
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto lineas = parsearFichero lineas >>= validarNivel

-- BLOQUE 3 
-- Subtarea 6A

tamCelda :: Float
tamCelda = 40

-- posicionCelda
posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j = 
    let x = fromIntegral j * tamCelda
        y = fromIntegral (total - 1 - i) * tamCelda
    in (x, y)

-- gridAPlataformas
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas grid = 
    let totalFilas = length grid
    in [ Plataforma (x, y, tamCelda, tamCelda)
       | (i, fila) <- zip [0..] grid
       , (j, celda) <- zip [0..] fila
       , esSolido celda
       , let (x, y) = posicionCelda totalFilas i j
       ]

-- metasDesdeGrid
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid grid = 
    let totalFilas = length grid
    in [ Meta (posicionCelda totalFilas i j)
       | (i, fila) <- zip [0..] grid
       , (j, celda) <- zip [0..] fila
       , esMeta celda
       ]

-- enemigosDesdeGrid
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid grid = 
    let totalFilas = length grid
    in [ Enemigo ident (x + tamCelda / 2, y + tamCelda / 2) (-40, 0) (tamCelda, tamCelda) Patrulla
       | (i, fila) <- zip [0..] grid
       , (j, celda) <- zip [0..] fila
       , InicioEnemigo identStr <- [celda]
       , let ident = head identStr
             (x, y) = posicionCelda totalFilas i j
       ]
