import Test.HUnit
import Palet

import Route


route1 = newR ["roma", "paris", "mdq" , "berna"]
route2 = newR ["roma"]


testNewRempty :: Test
testNewRempty = TestCase (assertEqual "Si se puede crear una nueva Route vacia" (newR []) (newR []))

testinOrderR1 :: Test
testinOrderR1 = TestCase (assertEqual "Si calcula bien una ciudad anterior a otra" True (inOrderR route1 "paris" "berna"))

testinOrderR2 :: Test
testinOrderR2 = TestCase (assertEqual "Si calcula bien una ciudad anterior a otra" False (inOrderR route1 "mdq" "roma"))

testinOrderR3 :: Test
testinOrderR3 = TestCase (assertEqual "Si una ciudad no esta en la ruta no sabe si esta antes" False (inOrderR route2 "roma" "paris"))



runTests :: IO Counts
runTests = runTestTT (TestList [testNewRempty, testinOrderR1, testinOrderR2, testinOrderR3])

main :: IO ()
main = do
  counts <- runTests
  print counts