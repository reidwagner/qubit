module NQubit (tensor) where

import Data.Complex
import Data.List
import Data.List.Split
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
twoQBasis = chunksOf 4 $ [k0,k1] >>= \s0 -> [k0,k1] >>= \s1 -> tensor s0 s1

--There has to be a simpler way. This is unwieldy. Not functioning. 
nQBasis :: Int -> [State] -> [State]
nQBasis 2 xs = [[k0,k1] >>= (\k -> xs >>= (\s -> tensor k s))]
nQBasis n xs = chunksOf (n^2) $ concat $ nQBasis (n-1) [[k0,k1] >>= (\k -> xs >>= (\s -> tensor k s))]

