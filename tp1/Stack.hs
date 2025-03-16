{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Stack ( Stack, newS, freeCellsS, stackS, netS, holdsS, popS )
  where

import Palet
import Route

data Stack = Sta [ Palet ] Int deriving (Eq, Show)

-- createpalets :: Int -> [Palet]
-- createpalets x = replicate x (newP "" 0)

newS :: Int -> Stack                      -- construye una Pila con la capacidad indicada 
newS capacity = Sta [] capacity

freeCellsS :: Stack -> Int                -- responde la celdas disponibles en la pila
freeCellsS (Sta palets _) = length (filter (\palet -> netP palet == 0) palets)

stackS :: Stack -> Palet -> Stack         -- apila el palet indicado en la pila
stackS (Sta palets capacity) palet
  | length palets < capacity = Sta (palet : palets) capacity -- CUBRIR TEMA PESO
  | otherwise = error "Stack is full"

netS :: Stack -> Int                      -- responde el peso neto de los paletes en la pila
netS (Sta palets _) = sum (map netP palets)

holdsS :: Stack -> Palet -> Route -> Bool
holdsS stack new_palet route =
    freeCellsS stack > 0 && inOrderR route (destinationP new_palet) (destinationP (lastPalet stack))

-- Función auxiliar para obtener el último palet en la pila (el de arriba)
lastPalet :: Stack -> Palet
lastPalet (Sta [] _) = error "Stack is empty"
lastPalet (Sta palets _) = last palets

popS :: Stack -> String -> Stack
popS (Sta palets capacity) city = Sta (filter (\palet -> destinationP palet /= city) palets) capacity
   -- quita del tope los paletes con destino en la ciudad indicada

{-
TESTEO
PONER MAS PALETS D LA CANT DISPONIBLES (CANTIDAD)
PONER MAS PESO DEL QUE HAY
chequear pops filtro: que pasa si no hay palet para esa ciudad
-}