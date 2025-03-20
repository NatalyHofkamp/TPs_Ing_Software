import Test.HUnit
import Palet


testDestionationP :: Test
testDestionationP = TestCase (assertEqual "Si puede devolver la ciudad destino de un palet" "Rio Primero" (destinationP (newP "Rio Primero" 3)))

testNetP :: Test
testNetP = TestCase (assertEqual "Si se puede devolver el peso de un palet" 6 (netP (newP "Rio Segundo" 6)))


testNetP2 :: Test
testNetP2 = TestCase (assertEqual "Si se puede crear un Palet con peso negativo" (-6) (netP (newP "Rio Tercero" (-6))))

testNetP3 :: Test
testNetP3 = TestCase (assertEqual "Si se puede crear un Palet con peso cero" 0 (netP (newP "Rio Cuarto" 0)))


runTests :: IO Counts
runTests = runTestTT (TestList [ testDestionationP, testNetP,testNetP2, testNetP3])

main :: IO ()
main = do
  counts <- runTests
  print counts
