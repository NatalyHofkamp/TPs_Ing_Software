{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Route ( Route, newR, inOrderR )
  where 

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


inOrderR :: Route -> String -> String -> Bool
inOrderR (Rou (c:cs)) city1 city2
    | c == city1 = city2 `elem` cs  -- Si encontramos city1, verificamos si city2 está después en la lista
    | c == city2 = False            -- Si encontramos city2 antes que city1, retornamos False
    | otherwise = inOrderR (Rou cs) city1 city2  -- Seguimos buscando en la lista
inOrderR _ _ _ = False  -- Caso base: lista vacía, retorna False
