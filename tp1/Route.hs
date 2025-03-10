
module Route ( Route, newR, inOrderR )
  where 

data Route = Rou [ String ] deriving (Eq, Show)

newR :: [ String ] -> Route                    -- construye una ruta segun una lista de ciudades
newR cities = Rou cities

findIndex :: Route -> String -> Int 
findIndex i1 | i1<-[0..length(route)] , (!!) route i1 = city1 
             |  otherwise = -1 


inOrderR :: Route -> String -> String -> Bool
inOrderR route city1 city2 = i1 /= -1 && i2 /= -1 && i1 < i2
  where
    i1 = findIndex route city1
    i2 = findIndex route city2
