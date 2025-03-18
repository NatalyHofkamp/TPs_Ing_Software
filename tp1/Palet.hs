{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Palet ( Palet, newP, destinationP, netP )
  where

import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (evaluate, tryJust, SomeException)

data Palet = Pal String Int deriving (Eq, Show)

newP :: String -> Int -> Palet   -- construye un Palet dada una ciudad de destino y un peso en toneladas
newP city weight = Pal city weight

destinationP :: Palet -> String  -- responde la ciudad destino del palet
destinationP (Pal city _) = city

netP :: Palet -> Int -- responde el peso en toneladas del palet
netP (Pal _ weight) = weight 


testF :: Show a => a -> Bool
testF action = unsafePerformIO $ do
    result <- tryJust isException (evaluate action)
    return $ case result of
        Left _ -> True
        Right _ -> False
    where
        isException :: SomeException -> Maybe ()
        isException _ = Just () 


testNewP :: String -> Int -> Bool
testNewP city weight= testF (newP city weight) == False

testDestionationP :: Palet -> Bool 
testDestionationP (Pal city _) = testF (destinationP (Pal city 0)) == False
-- Asegúrate de que destinationP no lance excepciones al pasar un Palet con una ciudad (y un peso arbitrario de 0)

testNetP :: Palet -> Bool 
testNetP (Pal _ weight) = testF (netP (Pal "" weight)) == False
-- Aquí estamos pasando un Palet con un peso (y una ciudad vacía) para verificar si netP lanza excepciones
