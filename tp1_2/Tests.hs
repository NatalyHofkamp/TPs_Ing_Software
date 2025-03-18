import Test.HUnit
import Stack
import Palet
import Route

-- Crear palets de prueba
p1 :: Palet
p1 = Pal "Madrid" 5

p2 :: Palet
p2 = Pal "Barcelona" 1

p3 :: Palet
p3 = Pal "Valencia" 3

-- Crear una pila de prueba con capacidad 3
stack1 :: Stack
stack1 = Sta [p1, p2] 3

stack2 :: Stack
stack2 = Sta [p1, p2, p3] 3  -- Pila con capacidad llena

-- Test de freeCellsS
testFreeCellsS :: Test
testFreeCellsS = TestCase (assertEqual "Celdas disponibles" 1 (freeCellsS stack1))

-- Test de netS
testNetS :: Test
testNetS = TestCase (assertEqual "Peso neto de la pila" 6 (netS stack1))

-- Test de getLastP
testGetLastP :: Test
testGetLastP = TestCase (assertEqual "Último palet de la pila" p2 (getLastP stack1))

-- Test de holdsS
testHoldsS :: Test
testHoldsS = TestCase (assertEqual "Si se puede apilar el palet en la pila" True (holdsS stack1 p3 route1))

-- Test de stackS
testStackS :: Test
testStackS = TestCase (assertEqual "Agregar un palet a la pila" (Sta [p3, p1, p2] 3) (stackS stack1 p3))

-- Test de popS
testPopS :: Test
testPopS = TestCase (assertEqual "Quitar un palet de la pila" (Sta [p1,p2] 3) (popS stack1 "Valencia"))

-- Crear una ruta de prueba para `holdsS`
route1 :: Route
route1 = Rou ["Madrid", "Barcelona", "Valencia"]

-- Función para ejecutar todos los tests
runTests :: IO Counts
runTests = runTestTT (TestList [testFreeCellsS, testNetS, testGetLastP, testHoldsS, testStackS, testPopS])

main :: IO ()
main = do
  counts <- runTests
  print counts
