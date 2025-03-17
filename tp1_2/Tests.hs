import Test.HUnit
import Palet  -- Importa el módulo donde definiste `Palet`
import Route

-- Definir algunos valores de prueba
p1 = Pal "Madrid" 500
p2 = Pal "Barcelona" 750

-- Casos de prueba para destinationP
testDestinationP = TestList [
    TestCase (assertEqual "Destino de p1" "Madrid" (destinationP p1)),
    TestCase (assertEqual "Destino de p2" "Barcelona" (destinationP p2))
    ]

-- Casos de prueba para netP
testNetP = TestList [
    TestCase (assertEqual "Peso de p1" 500 (netP p1)),
    TestCase (assertEqual "Peso de p2" 750 (netP p2))
    ]

-- Definimos algunas rutas de prueba
route1 = Rou ["Madrid", "Barcelona", "Valencia", "Sevilla"]
route2 = Rou ["Paris", "Lyon", "Marseille"]
route3 = Rou ["Berlin", "Munich", "Hamburg", "Cologne"]

-- Pruebas para `getCity`
testGetCity = TestList [
    TestCase (assertEqual "Madrid antes que Valencia" True (getCity ["Madrid", "Barcelona", "Valencia"] "Madrid" "Valencia")),
    TestCase (assertEqual "Valencia antes que Madrid" False (getCity ["Valencia", "Barcelona", "Madrid"] "Madrid" "Valencia")),
    TestCase (assertEqual "Ciudad inexistente" False (getCity ["Madrid", "Barcelona", "Valencia"] "Lisboa" "Valencia"))
  ]

-- Pruebas para `inOrderR`
testInOrderR = TestList [
    TestCase (assertEqual "Madrid antes que Valencia" True (inOrderR route1 "Madrid" "Valencia")),
    TestCase (assertEqual "Valencia antes que Madrid" False (inOrderR route1 "Valencia" "Madrid")),
    TestCase (assertEqual "Ciudad no en la ruta" False (inOrderR route1 "Lisboa" "Valencia")),
    TestCase (assertEqual "Primera ciudad antes que segunda en otra ruta" True (inOrderR route2 "Paris" "Marseille")),
    TestCase (assertEqual "Ciudad inexistente en otra ruta" False (inOrderR route2 "Berlin" "Lyon"))
  ]


-- Ejecutar todas las pruebas
main :: IO ()
main = do
    runTestTT testDestinationP
    runTestTT testNetP
    runTestTT testGetCity
    runTestTT testInOrderR
    return ()
