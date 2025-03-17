module Route ( Route(..), newR, inOrderR,getCity)
  where
import Data.List (elemIndex)

data Route = Rou [ String ] deriving (Eq, Show)

newR :: [ String ] -> Route -- construye una ruta segun una lista de ciudades
newR cities = Rou cities

inRouteR :: Route -> String -> Bool -- indica si la ciudad consultada está en la ruta
inRouteR (Rou cs) city  = city `elem` cs

getCity :: [String] -> String -> String -> Bool
getCity [] _ _ = False  -- Si no quedan elementos en la lista, devuelve False
getCity (x:xs) city1 city2
  | x == city1 = True  -- Si encontramos city1, devolvemos True
  | x == city2 = False  -- Si encontramos city2 antes que city1, devolvemos False
  | otherwise = getCity xs city1 city2  -- Continuamos la búsqueda con el resto de la lista


inOrderR :: Route -> String -> String -> Bool
inOrderR (Rou cs) city1 city2 
    | not (inRouteR (Rou cs) city1) = False  -- Si city1 no está en la ruta, retorna False
    | not (inRouteR (Rou cs) city2) = False  -- Si city2 no está en la ruta, retorna False
    | otherwise = getCity (reverse cs) city1 city2  -- Recorremos la lista de ciudades en orden inverso
