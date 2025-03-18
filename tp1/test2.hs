import Test.HUnit
import Palet
testDestinationP :: Palet -> Palet -> Bool
testDestinationP p1 p2 = and [
    destinationP p1 == "Madrid",    -- Verifica si el destino de p1 es Madrid
    destinationP p2 == "Barcelona"  -- Verifica si el destino de p2 es Barcelona
  ]

p1 = newP "Madrid" 2
p2 = newP "Barcelona" 3
-- Definir los objetos de prueba en la función main
main :: IO ()
main = do
  
  
  -- Ejecutar la prueba y mostrar el resultado
  putStrLn (show (testDestinationP p1 p2))