import Test.HUnit
import Palet

import Route


route1 = newR ["roma", "paris", "mdq" , "berna"]
route2 = newR ["roma"]


testNewRempty :: Test
testNewRempty = TestCase (assertEqual "Si se puede crear una nueva Route vacia" (newR []) (newR []))

testNewR1 :: Test
testNewR1= TestCase (assertEqual "Si se puede crear una nueva Route" (newR ["dublin", "belfast", "derry"]) (newR ["dublin", "belfast", "derry"]))

-- testNewPNeg :: Test
-- testNewPNeg = TestCase (assertEqual "Debe lanzar error con peso negativo" (error "Invalid weight: must be greater than 0") (newP "Bariloche" (-4)))

testNewR2 :: Test
testNewR2 = TestCase (assertEqual "Si se puede crear una nueva Route" (newR ["dublin", "dublin", "derry"]) (newR ["dublin", "dublin", "derry"]))

testNewR3 :: Test
testNewR3 = TestCase (assertEqual "Si se puede crear una nueva Route" (newR ["dublin", "derry", "dublin"]) (newR ["dublin", "derry", "dublin"]))


testinOrderR1 :: Test
testinOrderR1 = TestCase (assertEqual "Si sesklajdlkjsd" True (inOrderR route1 "paris" "berna"))

testinOrderR2 :: Test
testinOrderR2 = TestCase (assertEqual "Si se puede crear un nuevo Palet" False (inOrderR route1 "mdq" "roma"))

testinOrderR3 :: Test
testinOrderR3 = TestCase (assertEqual "Si se puede crear un nuevo Palet" False (inOrderR route2 "roma" "paris"))



runTests :: IO Counts
runTests = runTestTT (TestList [testNewRempty, testNewR1,testNewR2,testNewR3, testinOrderR1, testinOrderR2, testinOrderR3])

main :: IO ()
main = do
  counts <- runTests
  print counts