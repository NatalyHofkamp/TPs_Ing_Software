module Truck ( Truck (..), newT, freeCellsT, loadT, unloadT, netT )
  where

import Palet
import Stack
import Route
data Truck = Tru [ Stack ] Route deriving (Eq, Show)

newT :: Int -> Int -> Route -> Truck -- construye un camion según una cantidad de bahias, la altura de las mismas y una ruta
newT q_bahias height = Tru (replicate q_bahias (newS height))

freeCellsT :: Truck -> Int            -- responde la celdas disponibles en el camion
freeCellsT (Tru stacks _) = sum (map freeCellsS stacks)


loadT :: Truck -> Palet -> Truck
loadT (Tru (s:ss) route) palet
    | stackS s palet /= s = Tru ((stackS s palet) : ss) route  -- Si el palet se pudo apilar, actualiza el stack
    | otherwise = let Tru newStacks newRoute = loadT (Tru ss route) palet  -- Intenta en los stacks restantes
                  in Tru (s : newStacks) newRoute  -- Mantiene el stack original si no se pudo cargar ahí



unloadT :: Truck -> String -> Truck   -- responde un camion al que se le han descargado los paletes que podían descargarse en la ciudad
unloadT (Tru stacks route) city = Tru (map (`popS` city) stacks) route

netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion
netT (Tru stacks _) = sum (map netS stacks)
