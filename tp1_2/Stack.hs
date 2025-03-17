module Stack ( Stack (..), newS, nfreeCellsS, stackS, netS, holdsS, popS,getLastP)
  where

import Palet
import Route

data Stack = Sta [ Palet ] Int deriving (Eq, Show)

newS :: Int -> Stack                      -- construye una Pila con la capacidad indicada 
newS capacity = Sta [] capacity

nfreeCellsS :: Stack -> Int                -- responde la celdas disponibles en la pila
nfreeCellsS (Sta palets capacity) = capacity - length  palets

netS :: Stack -> Int                      -- responde el peso neto de los paletes en la pila
netS (Sta palets _) = sum (map netP palets)

getLastP :: Stack -> Palet -- devuelve el último palet en la pila
getLastP (Sta [] _) = error "La pila está vacía"  -- Si la pila está vacía, lanza un error
getLastP (Sta [x] _) = x  -- Si solo queda un palet en la pila, lo devolvemos
getLastP (Sta (_:xs) _) = getLastP (Sta xs undefined)  -- Recursión con el resto de la lista


holdsS :: Stack -> Palet -> Route -> Bool
holdsS stack new_palet route =
    inOrderR route (destinationP new_palet) (destinationP (getLastP stack))


getCities :: Stack -> [String] -- Devuelve una lista de todas las ciudades de los palets en la pila.
getCities (Sta palets _) = [destinationP p | p <- palets]  -- Recoge las ciudades de los palets en la pila



stackS :: Stack -> Palet -> Stack
stackS (Sta palets capacity) palet
  | length palets < capacity  -- Verifica que no se haya alcanzado la capacidad
    && netS (Sta palets capacity) + netP palet <= 10  -- Verifica el peso total
    && (destinationP palet `notElem` cities  -- Verifica que la ciudad de destino no esté ya en la pila
        || null palets
        || getCity (reverse cities) (destinationP palet) (destinationP (getLastP (Sta palets capacity))))  -- Verifica el orden de las ciudades
  = Sta (palet : palets) capacity  -- Si cumple todas las condiciones, agrega el palet
  | otherwise = Sta palets capacity  -- Si no se puede agregar, retorna la pila sin cambios
  where
    cities = getCities (Sta palets capacity)  
    
popS :: Stack -> String -> Stack  -- quita del tope los paletes con destino en la ciudad indicada
popS (Sta palets capacity) city = Sta [p | p <- palets, destinationP p /= city] capacity


