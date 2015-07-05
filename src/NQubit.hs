{-# OPTIONS_GHC -fno-warn-tabs #-}

module NQubit (tensor) where

import Data.Complex
import Data.List
import Data.List.Split
import Qubit

type State = [[Complex Float]]
type Operator = [[Complex Float]]

k0 = kets!!0
k1 = kets!!1

-- Tensor product. (x)  
tensor :: State -> State -> State
tensor k0 k1 = transpose [[(*)] <*> concat k0 <*> concat k1]

--No capitals for function name. Compiler won't tell you. 

--Initializes two qubit basis. 
twoQBasis :: [State]
twoQBasis = [k0,k1] >>= \s0 -> [k0,k1] >>= \s1 -> [tensor s0 s1]

--Can I work around this? Starting point for creating a basis. 
genBasis :: Int -> [State] 
genBasis n = nQBasis n [k0,k1]

--Working. Creates n qubit basis. 
nQBasis :: Int -> [State] -> [State]
nQBasis 1 xs = [k0,k1]
nQBasis 2 xs = [k0,k1] >>= (\k -> xs >>= (\s -> [tensor k s]))
nQBasis n xs = nQBasis (n-1) [k0,k1] >>= (\k -> xs >>= (\s -> [tensor k s]))

showH :: State -> String
showH k = "[" ++ (matToString $ transpose $ k) ++ "]"

showBasis :: [State] -> IO()
showBasis state = mapM_ (putStrLn) (map showH state)

--2QB swap gate.
swap :: Operator
swap = [[1,0,0,0],
	[0,0,1,0],
	[0,1,0,0],
	[0,0,0,1]]

--2QB cnot gate. 
cnot :: Operator 
cnot = [[1,0,0,0],
	[0,1,0,0],
	[0,0,0,1],
	[0,0,1,0]]

