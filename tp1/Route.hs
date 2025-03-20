module Route ( Route, newR, inOrderR, inRouteR)
  where
import Data.List (elemIndex)

data Route = Rou [ String ] deriving (Eq, Show)

newR :: [ String ] -> Route -- construye una ruta segun una lista de ciudades
newR cities = Rou cities

inRouteR :: Route -> String -> Bool -- indica si la ciudad consultada está en la ruta
inRouteR (Rou cs) city  = city `elem` cs

getCity :: [String] -> String -> String -> Bool 
getCity [] _ _ = False  
getCity (x:xs) city1 city2
  | x == city1 = True 
  | x == city2 = False 
  | otherwise = getCity xs city1 city2  

inOrderR :: Route -> String -> String -> Bool
inOrderR (Rou cs) city1 city2
    | not (inRouteR (Rou cs) city1) = False  
    | not (inRouteR (Rou cs) city2) = False 
    | otherwise = getCity (reverse cs) city1 city2  
