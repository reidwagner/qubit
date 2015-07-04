module Main (main) where

import Qubit
import Test.Hspec
import Data.Complex

main = do
        putStrLn (super z0)
        putStrLn "Applying Hadamard gate.."
        putStrLn (super (operate z0 had))
