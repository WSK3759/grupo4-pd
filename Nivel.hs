

module Nivel
    ( ErrorNivel(..), NivelData(..)
    , buscarClave, splitOn
    , parsearFichero, validarNivel, parsearNivelCompleto
    , tamCelda, posicionCelda
    , gridAPlataformas, metasDesdeGrid, enemigosDesdeGrid
    , parsearNiveles, todasLasPosiciones
    ) where

import Data.List (stripPrefix)
import Tipos
import Text.Read (readMaybe)

import Objeto (Plataforma(..), Meta(..), Enemigo(..))
------------------------------------------------
-- Bloque 2 - Monadas (3.0 puntos)
------------------------------------------------

-- SUBTAREA 4 - La monada Maybe: partidas guardadas
-- y tabla de puntuaciones (1.25 puntos)

-- buscarClave
-- Busca una clave en una lista de lineas y devuelve el texto
-- que aparece despues de ella en la primera linea que coincida.
-- Si no encuentra la clave, devuelve Nothing.
buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (linea:resto) =
    case stripPrefix clave linea of
        Just valor -> Just valor
        Nothing -> buscarClave clave resto


-- SUBTAREA 4 - Funcion auxiliar splitOn (como en la tarea 1)
-- Divide una cadena en partes usando un caracter separador.
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
    (a, [])   -> [a]
    (a, _:b)  -> a : splitOn c b


------------------------------------------------
-- Bloque 2 - Monadas (3.0 puntos)
------------------------------------------------

-- SUBTAREA 5 - La monada Either: interpretar y validar un nivel (1.75 puntos)

-- Representa los posibles errores al interpretar un nivel.
data ErrorNivel = FormatoInvalido String
                | NivelSinMeta
                | NivelConVariasMetas Int
                deriving (Eq, Show)

-- Representa los datos de un nivel: nombre, fondos y mapa.
data NivelData = NivelData
    { nombreNivel :: String
    , fondos      :: [Int]
    , gridNivel   :: Grid
    }
    deriving (Eq, Show)


-- parsearFichero
-- Interpreta las lineas de un nivel y devuelve sus datos.
-- Si falta algun elemento o hay datos invalidos, devuelve el error.
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero lineas = do

    -- Separamos la cabecera y el mapa buscando "---"
    (cabecera, mapa) <-
        case break (== "---") lineas of
            (_, []) -> Left (FormatoInvalido "falta el separador '---'")
            (cab, _:filas)
                | null filas -> Left (FormatoInvalido "falta el mapa")
                | otherwise -> Right (cab, filas)

    -- Buscamos el nombre del nivel
    nombre <-
        case buscarClave "NOMBRE: " cabecera of
            Nothing -> Left (FormatoInvalido "falta la clave 'NOMBRE: '")
            Just n  -> Right n

    -- Buscamos los fondos del nivel
    fondosTexto <-
        case buscarClave "FONDOS: " cabecera of
            Nothing -> Left (FormatoInvalido "falta la clave 'FONDOS: '")
            Just f  -> Right f

    -- Separamos los fondos y los convertimos a numeros
    listaFondos <-
        case mapM readMaybe (splitOn ',' fondosTexto) of
            Nothing -> Left (FormatoInvalido "FONDOS no numerico")
            Just fs -> Right fs

    -- Construimos el nivel con los datos obtenidos
    return (NivelData nombre listaFondos (parsearGrid mapa))


-- validarNivel
-- Comprueba que un nivel tenga exactamente una meta.
-- Si no tiene ninguna o tiene varias, devuelve el error correspondiente.
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nivel
    | cantidad == 0 = Left NivelSinMeta
    | cantidad > 1 = Left (NivelConVariasMetas cantidad)
    | otherwise = Right nivel
    where
        -- Recorremos todas las filas y contamos las celdas que son META.
        cantidad = length [c | fila <- gridNivel nivel,
                               c <- fila, esMeta c]


-- parsearNivelCompleto
-- Primero interpreta los datos del fichero y despues valida el nivel.
-- Si alguna funcion devuelve un error, se conserva ese error.
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto lineas =
    parsearFichero lineas >>= validarNivel


------------------------------------------------
-- Bloque 3 - Gloss: del mapa al mundo jugable
------------------------------------------------

-- SUBTAREA 6A - Del mapa a los objetos (0.75 puntos)

-- Tamaño de cada celda del mapa.
tamCelda :: Float
tamCelda = 40

-- posicionCelda
-- Calcula la posicion de una celda segun su fila y columna.
-- La primera fila del texto es la mas alta del mapa.
posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j =
    (fromIntegral j * tamCelda,
     fromIntegral (total - 1 - i) * tamCelda)


-- gridAPlataformas
-- Recorre el mapa y crea una plataforma por cada celda solida.
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas grid =
    [ Plataforma (x,y,tamCelda,tamCelda)
    | (i,fila) <- zip [0..] grid
    , (j,celda) <- zip [0..] fila
    , esSolido celda
    , let (x,y) = posicionCelda (length grid) i j
    ]


-- metasDesdeGrid
-- Recorre el mapa y crea una Meta en cada celda META.
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid grid =
    [ Meta (posicionCelda (length grid) i j)
    | (i,fila) <- zip [0..] grid
    , (j,celda) <- zip [0..] fila
    , esMeta celda
    ]

-- enemigosDesdeGrid
-- Crea un enemigo por cada InicioEnemigo del mapa.
-- Los enemigos se colocan en el centro de su celda.
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid grid =
    [ Enemigo ident (x+20,y+20) (-40,0) (tamCelda,tamCelda) Patrulla
    | (i,fila) <- zip [0..] grid
    , (j,InicioEnemigo ident) <- zip [0..] fila
    , let (x,y) = posicionCelda (length grid) i j
    ]


------------------------------------------------
-- BONUS - Functor, Applicative y Traversable
------------------------------------------------

-- BONUS 3 - parsearNiveles (0.25 puntos)
-- Interpreta y valida una lista de niveles.
-- Devuelve todos los niveles validos o el primer error.
-- Utilizamos traverse con Either.
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles = traverse parsearNivelCompleto


-- BONUS 4 - todasLasPosiciones (0.25 puntos)
-- Devuelve todas las posiciones (fila, columna) de un Grid.
-- Utilizamos el Applicative de listas y no comprensiones.
todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones [] = []
todasLasPosiciones (fila:filas) =
    (,) <$> [0 .. length filas] <*> [0 .. length fila - 1]
