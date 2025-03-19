import Test.HUnit
import Stack
import Palet
import Route

-- Crear palets de prueba
p1 :: Palet
p1 = newP "Madrid" 5

p2 :: Palet
p2 = newP "Barcelona" 1

p3 :: Palet
p3 = newP "Valencia" 3

palets1 = [p1, p2]
palets2 = [p1, p2, p3]

apilarPalets :: Stack -> [Palet] -> Stack
apilarPalets stack [] = stack  -- Caso base: si no hay más palets, se devuelve el stack final
apilarPalets stack (p:ps) = apilarPalets (stackS stack p) ps  -- Apila y sigue con los restantes

stack1 = newS 3
palets = [p1, p2, p3]
stack1f = apilarPalets stack1 palets1

stack2 = newS 3
stack2f = apilarPalets stack2 palets1

stack3 = newS 3
stack3f = apilarPalets stack3 palets2

route1 :: Route
route1 = newR ["Madrid", "Barcelona", "Valencia"]


testNewS :: Test
testNewS = TestCase (assertEqual "Stack con capacidad negativa se crea en cero" (newS (-2)) (newS 0))

-- Test de freeCellsS
testFreeCellsS :: Test
testFreeCellsS = TestCase (assertEqual "Celdas disponibles" 1 (freeCellsS stack1f))

-- Test de netS
testNetS :: Test
testNetS = TestCase (assertEqual "Peso neto de la pila" 6 (netS stack1f))

-- -- Test de getLastP
-- testGetLastP :: Test
-- testGetLastP = TestCase (assertEqual "Último palet de la pila" p2 (getLastP stack1))

-- Test de holdsS
testHoldsS :: Test
testHoldsS = TestCase (assertEqual "Si se puede apilar el palet en la pila" True (holdsS stack1f p3 route1))

-- Test de stackS
testStackS :: Test
testStackS = TestCase (assertEqual "Agregar un palet a la pila" stack3f (stackS stack1f p3))

-- Test de popS
testPopS :: Test
testPopS = TestCase (assertEqual "Quitar un palet de la pila" stack2f (popS stack1f "Valencia"))


-- Función para ejecutar todos los tests
runTests :: IO Counts
runTests = runTestTT (TestList [testNewS, testFreeCellsS, testNetS, testHoldsS, testStackS, testPopS])

main :: IO ()
main = do
  counts <- runTests
  print counts
