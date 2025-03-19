module Stack ( Stack, newS, freeCellsS, stackS, netS, holdsS, popS )
  where

import Palet
import Route

data Stack = Sta [ Palet ] Int deriving (Eq, Show)

newS :: Int -> Stack                      -- construye una Pila con la capacidad indicada 
newS capacity = Sta [] capacity           --contempla caso de capacidad negativa? 

freeCellsS :: Stack -> Int                -- responde la celdas disponibles en la pila
freeCellsS (Sta palets capacity) = capacity - length  palets

netS :: Stack -> Int                      -- responde el peso neto de los palets en la pila
netS (Sta palets _) = sum (map netP palets)

getLastP :: [Palet] -> Palet --devuelve el último palet en el stack
getLastP [x] = x                 
getLastP (_:xs) = getLastP xs     
getLastP [] = error "Empty stack" 

getCities :: Stack -> [String] -- Devuelve una lista de todas las ciudades de los palets en la pila.
getCities (Sta palets _) = [destinationP p | p <- palets]  

holdsS :: Stack -> Palet -> Route -> Bool
holdsS (Sta [] _) _ _ = True
holdsS (Sta palets _) new_palet route =
  inOrderR route (destinationP new_palet) (destinationP (getLastP palets))

stackS :: Stack -> Palet -> Stack
stackS (Sta palets capacity) palet
  | netP palet > 0 
    && freeCellsS (Sta palets capacity) > 0
    && netS (Sta (palet :palets) capacity) <= 10
      = Sta (palets ++ [palet]) capacity  
  | otherwise = Sta palets capacity
  where
    cities = getCities (Sta palets capacity)

popS :: Stack -> String -> Stack  -- quita del tope los paletes con destino en la ciudad indicada
popS (Sta palets capacity) city = Sta [p | p <- palets, destinationP p /= city] capacity
