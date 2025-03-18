import Control.Exception
import System.IO.Unsafe

import Stack
import Palet
import Route


testF :: Show a => a -> Bool
testF action = unsafePerformIO $ do
    result <- tryJust isException (evaluate action)
    return $ case result of
        Left _ -> True
        Right _ -> False
    where
        isException :: SomeException -> Maybe ()
        isException _ = Just ()

route1 = newR ["roma"]
route2 = newR ["roma", "paris", "mdq" , "berna"]

p1 :: Palet
p1 = newP "Madrid" 5

p2 :: Palet
p2 = newP "Barcelona" 1

p3 :: Palet
p3 = newP "Valencia" 3

p4 :: Palet
p4 = newP "Madrid" 4

p5 :: Palet
p5 = newP "Valencia" 5

p6 :: Palet
p6 = newP "Dublin" 6

stack1 :: Stack
stack1 = Sta [p1, p2] 3

stack2 :: Stack
stack2 = Sta [p1, p2, p3] 3 

tests_palet = [testF(newP "Bariloche" 5), testF(newP "Bariloche" 0), testF(newP "Bariloche" (-2)), 
                testF(destinationP (newP "Dublin" 3)), testF(netP (newP "Rio Cuarto" 6))]

tests_route = [ testF(newR []), 
      testF(newR ["dublin"]),
      testF(inOrderR route1 "roma" "paris"), 
      testF(inOrderR route2 "roma" "mdq")
     ]

tests_stack = [testF(newS 9), testF(newS (-4)), testF(newS (-4)), 
              testF(freeCellsS stack1), testF(freeCellsS stack2),
              testF(netS stack1), testF(netS stack2),
              testF(holdsS stack1 p3 route1), testF(stackS stack1 p3)]


