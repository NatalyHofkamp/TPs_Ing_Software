{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Route ( Route, newR, inOrderR )
  where 
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (evaluate, tryJust, SomeException)
import Control.Exception (throw, Exception)
data Route = Rou [ String ] deriving (Eq, Show)

newR :: [ String ] -> Route                    -- construye una ruta segun una lista de ciudades
newR cities = Rou cities

-- findIndex :: Route -> String -> Int 
-- findIndex i1 | i1<-[0..length(route)] , (!!) route i1 = city1 
--              |  otherwise = -1 


-- inOrderR :: Route -> String -> String -> Bool
-- inOrderR route city1 city2 = i1 /= -1 && i2 /= -1 && i1 < i2
--   where
--     i1 = findIndex route city1
--     i2 = findIndex route city2
data CityNotFound = CityNotFound String
    deriving Show

instance Exception CityNotFound

inOrderR :: Route -> String -> String -> Bool
inOrderR (Rou cities) city1 city2
    | not (city1 `elem` cities) || not (city2 `elem` cities) = 
        throw (CityNotFound "Una de las ciudades no esta en la ruta")  -- Lanzamos una excepción si alguna ciudad no está en la lista
    | otherwise = checkOrder cities city1 city2
  where
    checkOrder (c:cs) city1 city2
        | c == city1 = city2 `elem` cs  -- Si encontramos city1, verificamos si city2 está después en la lista
        | c == city2 = False            -- Si encontramos city2 antes que city1, retornamos False
        | otherwise = checkOrder cs city1 city2  -- Seguimos buscando en la lista
    checkOrder [] _ _ = False 
inRouteR :: Route -> String -> Bool -- indica si la ciudad consultada está en la ruta
inRouteR (Rou cs) city  = city `elem` cs

testF :: Show a => a -> Bool
testF action = unsafePerformIO $ do
    result <- tryJust isException (evaluate action)
    return $ case result of
        Left _ -> True
        Right _ -> False
    where
        isException :: SomeException -> Maybe ()
        isException _ = Just () 


testNewR :: [String] -> Bool
testNewR cities = testF (newR cities) == False

testInOrderR :: Route -> String -> String -> Bool
testInOrderR route city1 city2 = testF (inOrderR route city1 city2) == False
