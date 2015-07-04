module Main (main) where

import Qubit
import Test.Hspec
import Data.Complex

main = do
	putStrLn "Begin with a spin down state:" 
        putStrLn (super z0)
        putStrLn "Applying Hadamard gate rotates the state pi about the (x+z)/sqrt(2) axis."
        putStrLn (super (operate z0 had))
	putStrLn "Applying Pauli-X gate rotates \960 about the X-axis. This is a quantum NOT gate."
	putStrLn (super (operate z0 sx)) 
	putStrLn "Applying Pauli-Y gate rotates \960 about the Y-axis."
	putStrLn (super (operate z0 sy))
	putStrLn "Applying Pauli-Z gate rotates \960 about the Z-axis. This is a phaseshift for \952 = \960."
	putStrLn (super (operate z0 sz))
	
