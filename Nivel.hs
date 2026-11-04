module Nivel ( ErrorNivel(..) , 
               NivelData(..) , 
               splitOn , 
               buscarClave , 
               parsearFichero , 
               validarNivel , 
               parsearNivelCompleto , 
               tamCelda , 
               posicionCelda , 
               gridAPlataformas , 
               metasDesdeGrid , 
               enemigosDesdeGrid,
               parsearNiveles,
               todasLasPosiciones 
            ) where 

import Tipos 
import Objeto 
import Data.List (stripPrefix, sortOn)
import Text.Read (readMaybe)

-- Subtarea 5 — La mónada Either: interpretar y validar un nivel

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

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])  -> [a]
  (a, _:b) -> a : splitOn c b

buscarClave :: String -> [String] -> Maybe String -- Dada una clave y una lista de claves, busca en dicha lista la primera línea que comience por el prefijo dado y devuelve el resto de esa linea.
buscarClave _ [] = Nothing
buscarClave clave (cl:lista) = case stripPrefix clave cl of
    Just resto -> Just resto
    Nothing    -> buscarClave clave lista


-- Funcion auxiliar maybeToEither para parsear tipo Maybe a Either usando patrones
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just v) = Right v
maybeToEither err Nothing = Left err

parsearFichero :: [String] -> Either ErrorNivel NivelData -- Dado un fichero de nivel (lineas), Devuelve un Either indicando si hay un error en el Nivel
parsearFichero ficheroNivel = do 
    let (cabecera, resto) = break (== "---") ficheroNivel -- 1. separamos la cabecera "---" del resto del mapa

    case resto of -- validamos que exista el separador "---"
        [] -> Left (FormatoInvalido "Falta la cabecera '---'")
        ("---": mapaTexto) -> do
            if null mapaTexto -- validamos que exista el mapa tras el separador
                then Left (FormatoInvalido "No hay mapa tras '---'")
                else do
                    -- 2. Extraemos la clave NOMBRE:
                    nombre <- maybeToEither (FormatoInvalido "Falta la clave 'NOMBRE: '") (buscarClave "NOMBRE: " cabecera)

                    -- 3. Extraemos la clave FONDOS: 
                    fondos <- maybeToEither (FormatoInvalido "Falta la clave 'FONDOS: '") (buscarClave "FONDOS: " cabecera)

                    -- 4. Convertir los fondos (lista separada por comas) a números [Int]
                    let listaFondos = splitOn ',' fondos 
                    fondosNum <- maybeToEither (FormatoInvalido "FONDOS contiene valores no numéricos") (mapM readMaybe listaFondos)

                    -- 5. Parseamos las celdas del mapa a Grid
                    let grid = parsearGrid mapaTexto

                    return (NivelData nombre fondosNum grid)

-- TAREA 2:

contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Int
contarCeldasQueCumplen condicion nivel = foldl sumarFila 0 nivel -- empiezo contando en cero y voy sumando las celdas válidas
  where
    sumarFila :: Int -> [Celda] -> Int
    sumarFila acumulador fila =
        acumulador + contarFila fila

    contarFila :: [Celda] -> Int
    contarFila fila = foldl contarCelda 0 fila

    contarCelda :: Int -> Celda -> Int
    contarCelda acumulador celda
      | condicion celda = acumulador + 1
      | otherwise = acumulador
                    
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nivelData 
    | numMetas == 0 = Left NivelSinMeta
    | numMetas > 1 = Left (NivelConVariasMetas numMetas)
    | otherwise = Right nivelData
    where
        numMetas = contarCeldasQueCumplen esMeta (gridNivel nivelData)

parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto nivel = parsearFichero nivel >>= validarNivel

-- BLOQUE 3 — Gloss: del mapa al mundo jugable
-- Subtarea 6 — Del mapa a los objetos, y de los objetos a la pantalla 

-- 6A. Del mapa a los objetos
tamCelda :: Float
tamCelda = 40 -- cada celda del mapa mide `tamCelda = 40` unidades.

posicionCelda :: Int -> Int -> Int -> Vector2D -- Dado un numero total de filas en un mapa, una fila i una columna j, calcula la posicion de la celda que se encuentra en esa posicion (i, j)
posicionCelda total i j = (realToFrac x, realToFrac y)
    where
        (x, y) = (fromIntegral j * tamCelda, fromIntegral (total-1-i) * tamCelda)

gridAPlataformas :: Grid -> [Plataforma] -- Dada un nivel (Lista de listas de Celda), devuelve una lista de Plataformas por cada celda Solido encontrada en el nivel
gridAPlataformas grd = [Plataforma (realToFrac x, realToFrac y, tamCelda, tamCelda) | (i, fila) <- zip [0..] grd, 
                                                                (j, celda) <- zip [0..] fila, 
                                                                esSolido celda, 
                                                                let (x,y) = posicionCelda (length grd) i j]

metasDesdeGrid :: Grid -> [Meta] -- Dado un nivel, devuelve la posicion de las celdas Metas de ese nivel
metasDesdeGrid grd = [Meta {posMeta = posM} | (i, fila) <- zip [0..] grd, 
                                              (j, celda) <- zip [0..] fila, 
                                              esMeta celda, 
                                              let posM = posicionCelda (length grd) i j]

enemigosDesdeGrid :: Grid -> [Enemigo] -- dado un nivel, devuelve los enemigos de ese nivel con la posicion de la celda en la que se encuentran
enemigosDesdeGrid grd = [Enemigo {identificador = show ident, 
                                  posicionE = posE, 
                                  velocidadE = (-40, 0), 
                                  dimension = (realToFrac tamCelda, realToFrac tamCelda), 
                                  tipoDeEnemigo = Patrulla} | (i, fila) <- zip [0..] grd, (j, ident) <- zip [0..] fila,
                                  esEnemigo ident,
                                  let posE = posicionCelda (length grd) i j]


-- BONUS
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData] -- módulo Nivel
parsearNiveles = traverse parsearNivelCompleto

todasLasPosiciones :: Grid -> [(Int, Int)]               -- módulo Nivel
todasLasPosiciones g = (,) <$> [0 .. length g - 1] <*> [0 .. numCols - 1] 
  where 
    numCols = if null g then 0 else length (head g)