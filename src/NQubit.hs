module NQubit (tensor) where

import Data.Complex
import Data.List
import Qubit

type State = [[Complex Float]]

k0 = kets!!0
k1 = kets!!1

-- Tensor product. (x)  
tensor :: State -> State -> State
tensor k0 k1 = transpose [[(*)] <*> concat k0 <*> concat k1]

--WOW. No capitals for function name. Compiler won't tell you. 

--Initializes two qubit basis. Probably a better way to do this with monads. 
twoQBasis :: [State]
twoQBasis = [tensor s0 s1 | s0 <- [k0,k1], s1 <- [k0,k1]]

--ThreeQBasis :: [State]
--ThreeQBasis = [k0,k1] >>= \x -> tensor k0 k1
