module TestsT where

import Truck

import Palet
import Route
import Stack
import Test.HUnit

-- Ejemplo de palets de prueba
p1 :: Palet
p1 = Pal "Madrid" 3 

p2 :: Palet
p2 = Pal "Valencia" 2 

p3 :: Palet
p3 = Pal "Barcelona" 1 

-- Ejemplo de ruta de prueba
route :: Route
route = Rou ["Madrid", "Valencia", "Barcelona"]

-- Creación de un camión de ejemplo
truck1 :: Truck
truck1 = newT 3 10 route -- Camión con 3 bahías y cada una con altura de 10

-- Casos de prueba

-- Test de freeCellsT
testFreeCellsT :: Test
testFreeCellsT = TestCase (assertEqual "Celdas disponibles en el camión" 30 (freeCellsT truck1))

-- Test de loadT (cargar palet en el camión)
testLoadT :: Test
testLoadT = TestCase (assertEqual "Cargar un palet en el camión" 
                      (Tru [Sta [p1] 10, Sta [p2] 10, Sta [p3] 10] route)
                      (loadT truck1 p1))

-- Test de unloadT (descargar palets de una ciudad)
testUnloadT :: Test
testUnloadT = TestCase (assertEqual "Descargar palets en la ciudad 'Valencia'" 
                      (Tru [Sta [p3] 10, Sta [p1] 10, Sta [p2] 10] route) 
                      (unloadT truck1 "Valencia"))

-- Test de netT (peso neto de los palets en el camión)
testNetT :: Test
testNetT = TestCase (assertEqual "Peso neto de los palets en el camión" 
                    6 (netT truck1))  -- 3 + 2 + 1

-- Test de cargar cuando no hay suficiente espacio
testLoadTNoSpace :: Test
testLoadTNoSpace = TestCase (assertEqual "Intento de cargar palet cuando no hay espacio"
                            truck1  -- El camión no cambia
                            (loadT truck1 p1))

-- Test de cargar en el segundo stack del camión
testLoadTSecondStack :: Test
testLoadTSecondStack = TestCase (assertEqual "Cargar en el segundo stack cuando el primero está lleno"
                                (Tru [Sta [p1] 10, Sta [p2, p3] 10, Sta [] 10] route)
                                (loadT (Tru [Sta [p2, p3] 10, Sta [] 10, Sta [] 10] route) p1))

-- Agrupar todos los tests
tests :: Test
tests = TestList [testFreeCellsT, testLoadT, testUnloadT, testNetT, testLoadTNoSpace, testLoadTSecondStack]

-- Ejecutar los tests
runTests :: IO Counts
runTests = runTestTT tests
