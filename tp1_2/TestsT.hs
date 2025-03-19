module TestsT where

import Test.HUnit
import Truck
import Palet
import Stack
import Route

-- Ejemplo de palets de prueba
p1 :: Palet
p1 = newP "Madrid" 3 

p2 :: Palet
p2 = newP "Valencia" 2 

p3 :: Palet
p3 = newP "Barcelona" 1 

p4 :: Palet
p4 = newP "Madrid" 4

p5 :: Palet
p5 = newP "Valencia" 5

p6 :: Palet
p6 = newP "Barcelona" 6

-- Ejemplo de ruta de prueba
route :: Route
route = newR ["Madrid", "Valencia", "Barcelona"]

route2 :: Route
route2 = newR ["Valencia", "Barcelona"]

routeEmpty :: Route
routeEmpty = newR[]

-- Camión de prueba con 3 bahías y capacidad de 10
truck1 :: Truck
truck1 = newT 3 10 route 

testNewT ::Test
testNewT = TestCase (assertEqual 
    "Se puede crear un nuevo camión con 9 bahías de altura 9 y una ruta dada" 
    (newT (replicate 9 (newS 9)) route) 
    (newT 9 9 route))

testNewT2 ::Test
testNewT2 = TestCase (assertEqual 
    "Se puede crear un nuevo camión con 9 bahías de altura 9 y una ruta dada" 
    (newT (replicate 9 (newS 9)) routeEmpty) 
    (newT 9 9 routeEmpty))

-- Test de freeCellsT
testFreeCellsT :: Test
testFreeCellsT = TestCase (assertEqual "Celdas disponibles en el camión" 30 (freeCellsT truck1))

-- Test cargar un palet en el camión vacío
testLoadTEmpty :: Test
testLoadTEmpty = TestCase (assertEqual "Cargar palet en camión vacío"
                      (newT [Sta [p1] 10, Sta [] 10, Sta [] 10] route)
                      (loadT truck1 p1))

-- Test cargar dos palets en el mismo stack
testLoadTStack :: Test
testLoadTStack = TestCase (assertEqual "Cargar dos palets en el mismo stack"
                      (newT [Sta [p1, p2] 10, Sta [] 10, Sta [] 10] route)
                      (loadT (loadT truck1 p1) p2))

-- Test intentar cargar cuando excede el peso permitido
testLoadTWeightLimit :: Test
testLoadTWeightLimit = TestCase (assertEqual "No cargar si excede el peso"
                      (newT [Sta [p1, p4] 10, Sta [p6] 10, Sta [] 10] route)  -- No cambia porque p4 sobrepasa el límite
                      (loadT(loadT (loadT truck1 p1) p4) p6))

-- Test descargar un palet correctamente
testUnloadT :: Test
testUnloadT = TestCase (assertEqual "Descargar palets en la ciudad 'Madrid'"
                      (newT [Sta [] 10, Sta [p2] 10, Sta [p3] 10] route2)
                      (unloadT (Tru [Sta [p1] 10, Sta [p2] 10, Sta [p3] 10] route) "Madrid"))

-- Test intentar descargar cuando la ciudad no tiene palets
testUnloadTNoPalets :: Test
testUnloadTNoPalets = TestCase (assertEqual "Intentar descargar donde no hay palets"
                      (newT [Sta [p1] 10, Sta [p2] 10, Sta [p3] 10] route)
                      (unloadT (Tru [Sta [p1] 10, Sta [p2] 10, Sta [p3] 10] route) "Sevilla"))  -- No hay palets en Sevilla

-- Test peso neto del camión
testNetT :: Test
testNetT = TestCase (assertEqual "Peso neto de los palets en el camión"
                      6  
                      (netT (Tru [Sta [p1] 10, Sta [p2] 10, Sta [p3] 10] route)))

testLoadUnloadMultipleTimes :: Test
testLoadUnloadMultipleTimes = TestCase (assertEqual "Cargar y descargar 5 paquetes dos veces"
    finalTruck
    truckAfterFirstUnload)
  where
    -- Camión inicial
    initialTruck = newT 3 10 route 
    -- Primera carga de 5 paquetes
    truckAfterFirstLoad = foldl loadT initialTruck [p1, p2, p3, p4, p5]
    -- Primera descarga de cada paquete
    truckAfterFirstUnload = foldl unloadT truckAfterFirstLoad ["Madrid", "Valencia", "Barcelona"]
    -- Estado esperado del camión al final (vacío porque se descargó todo)
    finalTruck = newT 3 10 routeEmpty

negTruck:: Truck
negTruck = newT 3 (-2) route 





runTests :: IO Counts
runTests = runTestTT (TestList [testNewT, testNewT2, testFreeCellsT, testLoadTEmpty, testLoadTStack,
                  testLoadTWeightLimit, testUnloadT, testUnloadTNoPalets, testNetT,testLoadUnloadMultipleTimes])

main :: IO ()
main = do
  counts <- runTests
  print counts

