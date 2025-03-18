import Test.HUnit
import Palet

-- -- Test de holdsS
-- testHoldsS :: Test
-- testHoldsS = TestCase (assertEqual "Si se puede apilar el palet en la pila" True (holdsS stack1 p3 route1))


testNewP :: Test
testNewP = TestCase (assertEqual "Si se puede crear un nuevo Palet" (Pal "Bariloche" 5) (newP "Bariloche" 5))

-- testNewPNeg :: Test
-- testNewPNeg = TestCase (assertEqual "Debe lanzar error con peso negativo" (error "Invalid weight: must be greater than 0") (newP "Bariloche" (-4)))



testDestionationP :: Test
testDestionationP = TestCase (assertEqual "Si se puede crear un nuevo Palet" "Dublin" (destinationP (newP "Dublin" 3)))

testNetP :: Test
testNetP = TestCase (assertEqual "Si se puede crear un nuevo Palet" 6 (netP (newP "Rio Cuarto" 6)))


testNetP2 :: Test
testNetP2 = TestCase (assertEqual "Si se puede crear un nuevo Palet" (-6) (netP (newP "Rio Cuarto" (-6))))

testNetP3 :: Test
testNetP3 = TestCase (assertEqual "Si se puede crear un nuevo Palet" 11 (netP (newP "Rio Cuarto" 11)))


runTests :: IO Counts
runTests = runTestTT (TestList [testNewP, testDestionationP, testNetP,testNetP2, testNetP3])

main :: IO ()
main = do
  counts <- runTests
  print counts
