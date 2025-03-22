import Test.HUnit
import Truck
import Palet
import Stack
import Route

-- Palets de prueba
p1, p2, p3, p4, p5, p6 :: Palet
p1 = newP "Pisa" 3 
p2 = newP "Bologna" 2 
p3 = newP "Catania" 3 
p4 = newP "Pisa" 4
p5 = newP "Venecia" 5
p6 = newP "Catania" 8
p7 = newP "Catania" 1
-- Rutas de prueba
route, route2, routeEmpty :: Route
route = newR ["Pisa", "Bologna", "Catania"]
route2 = newR ["Catania","Bologna"]
route3 = (newR ["Pisa", "Catania"])
routeEmpty = newR []

-- Camiones de prueba
truck1 = newT 3 10 route
truck2 = loadT truck1 p1 
truck3 = newT 1 10 route
truck4 = loadT truck3 p1 
truck5 = newT 1 10 routeEmpty
truck6 = newT 1 2 route2

truck7 = newT 3 2 route2

-- Prueba de creación de camión con ruta
testNewT :: Test
testNewT = TestCase (assertEqual 
    "Crear un camión con 3 bahías, altura 10 y ruta dada" 
    truck1 
    (newT 3 10 route))

-- Prueba de creación de camión sin ruta
testNewT2 :: Test
testNewT2 = TestCase (assertEqual 
    "Camión sin ruta se crea pero no carga palets" 
    (newT 3 10 routeEmpty) 
    (loadT (newT 3 10 routeEmpty) p1))

-- Test de celdas libres
testFreeCellsT :: Test
testFreeCellsT = TestCase (assertEqual "Celdas disponibles en el camión" 30 (freeCellsT truck1))

-- Test cargar un palet en el camión vacío
testLoadTEmpty :: Test
testLoadTEmpty = TestCase (assertEqual "Cargar palet en camión vacío"
                      truck2
                      (loadT truck1 p1))

testZeroCapacityTruck :: Test
testZeroCapacityTruck = TestCase (assertEqual "Camión con capacidad 0 no debe permitir carga"
                       (newT 0 10 route) 
                       (loadT (newT 0 10 route) p1)) 
                       
testLoadTStack :: Test
testLoadTStack = TestCase (assertEqual "Camión lleno no acepta más paquetes"
                      (loadT (loadT truck6 p2) p3) 
                      (loadT (loadT (loadT truck6 p2) p3) p7))


-- Test cargar un camion sin ruta
testLoadEmptyRoute :: Test
testLoadEmptyRoute = TestCase (assertEqual "No cargar si el camión tiene una ruta vacia"
                       truck5  
                       (loadT truck5 p2))


-- Test intentar cargar cuando excede el peso permitido
testLoadTWeightLimit :: Test
testLoadTWeightLimit = TestCase (assertEqual "No cargar si excede el peso"
                      truck4  
                      (loadT truck4 p6))

-- Test descargar un palet correctamente
testUnloadT :: Test
testUnloadT = TestCase (assertEqual "Descargar palets en la ciudad Pisa"
                      (unloadT truck2 "Pisa")
                      (newT 3 10 route))

-- Test intentar descargar donde no hay palets
testUnloadTNoPalets :: Test
testUnloadTNoPalets = TestCase (assertEqual "Intentar descargar donde no hay palets"
                      truck1  
                      (unloadT truck1 "Derry"))

-- Test de peso neto del camión
testNetT :: Test
testNetT = TestCase (assertEqual "Peso neto de los palets en el camión"
                      3  
                      (netT truck2))

-- Test de carga y descarga repetida
testLoadUnloadMultipleTimes :: Test
testLoadUnloadMultipleTimes = TestCase (assertEqual "Intento de 5 paquetes, carga de 3 y descarga de dos"
    finalTruck
    truckAfterFirstUnload)
  where
    initialTruck = newT 3 10 route3
    truckAfterFirstLoad = foldl loadT initialTruck [p1, p2, p3, p4, p5]
    truckAfterFirstUnload = unloadT truckAfterFirstLoad "Pisa"
    finalTruck =  loadT initialTruck p3


-- Test de camión con capacidad negativa
testInvalidTruck :: Test
testInvalidTruck = TestCase (assertEqual "Camión con capacidad negativa == camión con capacidad nula"
                      (newT 0 0 route)  
                      (newT (-2) 0 route))


-- Lista de pruebas
tests :: Test
tests = TestList [testNewT, testNewT2, testFreeCellsT, testLoadTEmpty,testZeroCapacityTruck, testLoadTStack,testLoadEmptyRoute,
                  testLoadTWeightLimit, testUnloadT, testUnloadTNoPalets, testNetT, 
                  testLoadUnloadMultipleTimes, testInvalidTruck]

main :: IO Counts
main = runTestTT tests
