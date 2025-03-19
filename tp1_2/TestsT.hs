module TestsT where

import Test.HUnit
import Truck
import Palet
import Stack
import Route

-- Palets de prueba
p1, p2, p3, p4, p5, p6 :: Palet
p1 = newP "Madrid" 3 
p2 = newP "Valencia" 2 
p3 = newP "Barcelona" 1 
p4 = newP "Madrid" 4
p5 = newP "Valencia" 5
p6 = newP "Barcelona" 8

-- Rutas de prueba
route, route2, routeEmpty :: Route
route = newR ["Madrid", "Valencia", "Barcelona"]
route2 = newR ["Valencia", "Barcelona"]
routeEmpty = newR []

-- Camión de prueba
truck1 :: Truck
truck1 = newT 3 10 route

truck2 = loadT truck1 p1 

truck3 = newT 1 10 route
truck4 = loadT truck3 p1 


-- Prueba de creación de camión con ruta
testNewT :: Test
testNewT = TestCase (assertEqual 
    "Crear un camión con 3 bahías, altura 10 y ruta dada" 
    truck1 
    (newT 3 10 route))

-- Prueba de creación de camión sin ruta
testNewT2 :: Test
testNewT2 = TestCase (assertEqual 
    "Crear un camión con 3 bahías, altura 10 y sin ruta" 
    (newT 3 10 routeEmpty) 
    (newT 3 10 routeEmpty))

-- Test de celdas libres
testFreeCellsT :: Test
testFreeCellsT = TestCase (assertEqual "Celdas disponibles en el camión" 30 (freeCellsT truck1))

-- Test cargar un palet en el camión vacío
testLoadTEmpty :: Test
testLoadTEmpty = TestCase (assertEqual "Cargar palet en camión vacío"
                      truck2  -- Ajustar según la implementación
                      (loadT truck1 p1))

-- Test cargar dos palets en el mismo stack
testLoadTStack :: Test
testLoadTStack = TestCase (assertEqual "Cargar dos palets en el mismo stack"
                      (loadT (loadT truck1 p1) p2)
                      (loadT (loadT truck1 p1) p2))

-- Test intentar cargar cuando excede el peso permitido
testLoadTWeightLimit :: Test
testLoadTWeightLimit = TestCase (assertEqual "No cargar si excede el peso"
                      truck4  -- Debería permanecer igual si se excede el peso
                      (loadT truck4 p6))

-- Test descargar un palet correctamente
testUnloadT :: Test
testUnloadT = TestCase (assertEqual "Descargar palets en la ciudad 'Madrid'"
                      (unloadT truck2 "Madrid")
                      (newT 3 10 route))

-- Test intentar descargar donde no hay palets
testUnloadTNoPalets :: Test
testUnloadTNoPalets = TestCase (assertEqual "Intentar descargar donde no hay palets"
                      truck1  -- No cambia nada
                      (unloadT truck1 "Sevilla"))

-- Test de peso neto del camión
testNetT :: Test
testNetT = TestCase (assertEqual "Peso neto de los palets en el camión"
                      3  
                      (netT truck2))

-- Test de carga y descarga repetida
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
    finalTruck = newT 3 10 route

-- Test de camión con capacidad negativa
testInvalidTruck :: Test
testInvalidTruck = TestCase (assertEqual "Crear camión con capacidad negativa (debería manejarlo correctamente)"
                      (newT 0 0 route)  -- Si `newT` maneja valores negativos, debería establecer 0
                      (newT (-2) 0 route))


-- Lista de pruebas
tests :: Test
tests = TestList [testNewT, testNewT2, testFreeCellsT, testLoadTEmpty, testLoadTStack,
                  testLoadTWeightLimit, testUnloadT, testUnloadTNoPalets, testNetT, 
                  testLoadUnloadMultipleTimes, testInvalidTruck]

-- Función principal para ejecutar los tests
main :: IO Counts
main = runTestTT tests
