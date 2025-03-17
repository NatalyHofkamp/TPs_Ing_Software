module Truck ( Truck (..), newT, freeCellsT, loadT, unloadT, netT )
  where

import Palet
import Stack
import Route
data Truck = Tru [ Stack ] Route deriving (Eq, Show)

newT :: Int -> Int -> Route -> Truck -- construye un camion según una cantidad de bahias, la altura de las mismas y una ruta
newT q_bahias height = Tru (replicate q_bahias (newS height))

freeCellsT :: Truck -> Int            -- responde la celdas disponibles en el camion
freeCellsT (Tru stacks _) = sum (map nfreeCellsS stacks)


loadT :: Truck -> Palet -> Truck      -- carga un palet en el camion
loadT (Tru [] route) _ = Tru [] route  -- Si no hay stacks, el camión no cambia
loadT (Tru (s:ss) route) palet
    | nfreeCellsS s > 0 = Tru (stackS s palet : ss) route  -- Carga en el primer stack con espacio
    | otherwise = let Tru newStacks _ = loadT (Tru ss route) palet  -- Prueba en los demás stacks
                  in Tru (s : newStacks) route

unloadT :: Truck -> String -> Truck   -- responde un camion al que se le han descargado los paletes que podían descargarse en la ciudad
unloadT (Tru stacks route) city = Tru (map (`popS` city) stacks) route

netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion
netT (Tru stacks _) = sum (map netS stacks)
