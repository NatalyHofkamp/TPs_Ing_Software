module Truck ( Truck, newT, freeCellsT, loadT, unloadT, netT )
  where

import Palet
import Stack
import Route
data Truck = Tru [ Stack ] Route deriving (Eq, Show)

newT :: Int -> Int -> Route -> Truck -- construye un camion según una cantidad de bahias, la altura de las mismas y una ruta
newT q_bahias height route = Tru (replicate q_bahias (newS height)) route

freeCellsT :: Truck -> Int            -- responde la celdas disponibles en el camion
freeCellsT (Tru stacks _) = sum (map freeCellsS stacks)

loadT :: Truck -> Palet -> Truck -- carga un palet en el camion
loadT (Tru [] route) palet = Tru [] route  
loadT (Tru (s:ss) route) (palet)
    | inRouteR  route  (destinationP palet)        
      && stackS s palet /= s   
      && holdsS s palet route               
      = Tru ((stackS s palet) : ss) route
    | otherwise = let Tru newStacks newRoute = loadT (Tru ss route) palet 
                  in Tru (s : newStacks) newRoute  

unloadT :: Truck -> String -> Truck
unloadT (Tru stacks route) city
    | inRouteR route city = Tru newStacks route
    | otherwise = Tru stacks route
  where
    newStacks = map (`popS` city) stacks 

netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion
netT (Tru stacks _) = sum (map netS stacks)