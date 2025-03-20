import Test.HUnit
import Palet

import Route


route1 = newR ["Wilde", "Bernal", "Quilmes" , "Banfield"]
route2 = newR ["Wilde"]


testNewRempty :: Test
testNewRempty = TestCase (assertEqual "Crear una nueva Route vacia" (newR []) (newR []))

testinOrderR1 :: Test
testinOrderR1 = TestCase (assertEqual "Una ciudad anterior a otra" True (inOrderR route1  "Banfield" "Bernal"))

testinOrderR2 :: Test
testinOrderR2 = TestCase (assertEqual "Una ciudad anterior a otra" False (inOrderR route1  "Wilde" "Quilmes"))

testinOrderR3 :: Test
testinOrderR3 = TestCase (assertEqual "Si una ciudad no esta en la ruta no sabe si esta antes" False (inOrderR route2 "Wilde" "Bernal"))



runTests :: IO Counts
runTests = runTestTT (TestList [testNewRempty, testinOrderR1, testinOrderR2, testinOrderR3])

main :: IO ()
main = do
  counts <- runTests
  print counts