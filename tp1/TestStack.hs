import Test.HUnit
import Stack
import Palet
import Route

-- Crear palets de prueba
p1 = newP "Cork" 5
p2 = newP "Galway" 1
p3 = newP "Belfast" 3

palets1 = [p1, p2]
palets2 = [p1, p2, p3]

apilarPalets :: Stack -> [Palet] -> Stack
apilarPalets stack [] = stack  -- Caso base: si no hay más palets, se devuelve el stack final
apilarPalets stack (p:ps) = apilarPalets (stackS stack p) ps  -- Apila y sigue con los restantes

stack1 = newS 3
stack1f = apilarPalets stack1 palets1

stack2 = newS 3
stack2f = apilarPalets stack2 palets1

stack3 = newS 3
stack3f = apilarPalets stack3 palets2

emptyStack = newS 3

route1 = newR ["Cork", "Galway", "Belfast"]


testNewS :: Test
testNewS = TestCase (assertEqual "Stack con capacidad negativa se crea en cero" (newS (-2)) (newS 0))

-- Test de freeCellsS
testFreeCellsS :: Test
testFreeCellsS = TestCase (assertEqual "Celdas disponibles" 1 (freeCellsS stack1f))

-- Test de netS
testNetS :: Test
testNetS = TestCase (assertEqual "Peso neto de la pila" 6 (netS stack1f))


-- Test de holdsS
testHoldsS :: Test
testHoldsS = TestCase (assertEqual "Si se puede apilar el palet en la pila" True (holdsS stack1f p3 route1))


testHoldsSWrongCity :: Test
testHoldsSWrongCity = TestCase (assertEqual "No permitir apilar palet si la ciudad no está en la ruta"
                    False
                    (holdsS stack1f (newP "Derry" 2) route1))

-- Test de stackS
testStackS :: Test
testStackS = TestCase (assertEqual "Agregar un palet a la pila" stack3f (stackS stack1f p3))

testStackFull :: Test
testStackFull = TestCase (assertEqual "No apilar si la pila está llena de palets"
                    stack3f  -- La pila debería mantenerse igual
                    (stackS stack3f p1))


testStackWeightLimit :: Test
testStackWeightLimit = TestCase (assertEqual "No apilar si la pila no aguanta más peso"
                    stack2f  -- La pila debería mantenerse igual
                    (stackS stack2f p1))


testHoldsSInvalidInsertion :: Test
testHoldsSInvalidInsertion = TestCase (assertEqual 
    "No permitir apilar un palet si rompe el orden de la ruta"
    False  -- No debería permitirse
    (holdsS stackWithC1C3 p2 route1))  -- Intentamos apilar un palet de "Galway"
  where
    -- Crear una pila con palets de "Cork" y "Belfast"
    stackWithC1C3 = apilarPalets (newS 3) [p1, p3]  -- `p1` = "Cork", `p3` = "Belfast"


testPopEmpty :: Test
testPopEmpty = TestCase (assertEqual "Quitar de una pila vacía no cambia nada"
                    emptyStack  -- La pila debería seguir vacía
                    (popS emptyStack "Cork"))
  where emptyStack = newS 3

-- Test de popS
testPopS :: Test
testPopS = TestCase (assertEqual "Quitar un palet de la pila" stack2f (popS stack1f "Belfast"))



-- Función para ejecutar todos los tests
runTests :: IO Counts
runTests = runTestTT (TestList [testNewS, testFreeCellsS, testNetS, testHoldsS,testHoldsSWrongCity, testStackS, testStackFull, testStackWeightLimit,testHoldsSInvalidInsertion, testPopS, testPopEmpty])

main :: IO ()
main = do
  counts <- runTests
  print counts
