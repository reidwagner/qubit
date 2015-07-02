module Qubit
( sz, sx, sy, had, l
, z0, z1, x0, x1, y0, y1
, super) 
where

import Data.Complex
import Data.List

type Ket = [[Complex Float]]
type Bra = [[Complex Float]] -- should be [Complex Float]
type Operator = [[Complex Float]]

--instance Show Ket where
--	show k = super k

--Kets
z0 = [[1 :+ 0],[0 :+ 0]] :: Ket
z1 = [[0],[1]] :: Ket
x0 = norm [[1],[1]] :: Ket
x1 = norm [[1],[-1]] :: Ket
y0 = norm [[1],[0 :+ 1]] :: Ket
y1 = norm [[1],[0 :+ 1]] :: Ket

--Gates
sz = [[1,0],[0,-1]] :: Operator
sx = [[0,1],[1,0]] :: Operator
sy = [[0,0 :+ (-1)],[0 :+ 1,0]] :: Operator
had = norm [[1,1],[1,-1]] :: Operator
l = [[1,0],[0,-1]] :: Operator

tensorproduct :: Ket -> Ket -> [[Complex Float]]
--Combining kets [(*)] <*> concat k1 <*> concat k2
--tensorproduct = [take 2, drop 2]

--Needs to be generalized
norm :: [[Complex Float]] -> [[Complex Float]]
norm z = let sq2 = 1/(sqrt 2.0) :: Float in zMatOp (*sq2) z -- [map (*sq2) xs | xs <- vect]

inner :: Bra -> Ket -> Complex Float
inner [[a],[b]] [[c],[d]] = a*c + b*d

operate :: Ket -> Operator -> Ket
operate [[x],[y]] [[a,b],[c,d]] =  [[(x*a)+(y*b)], [(x*c)+(y*d)]]

super :: Ket -> String
super k = (readComplex $ alpha k) ++ "|0> + " ++ (readComplex $ beta k) ++ "|1>"

alpha :: Ket -> Complex Float
alpha k = k!!0!!0

beta :: Ket -> Complex Float
beta k = k!!1!!0

--Read complex float. 
readComplex :: Complex Float -> String
readComplex z 
	| all (== 0) [real, imag] = "0" 
	| all (/= 0) [real, imag] = "(" ++ (show $ real) ++ " + " ++ (show $ imag) ++ "i)" 
	| real /= 0 = (show $ real) 
	| imag /= 0 = (show $ imag) ++ "i"
 	where 	real = roundDec $ realPart z
		imag = roundDec $ imagPart z

--Round to hundredth. 
roundDec :: Float -> Float
roundDec f = let c = 1000.0 :: Float in (fromInteger $ round $ f * (1000))/c 

--Complex float operation. Carries out float operation on each component. 
zFloatOp :: (Float -> Float) -> Complex Float -> Complex Float
zFloatOp f z = ((f $ realPart z) :+ (f $ imagPart z)) 

--Applies function to each element of matrix.  
zMatOp :: (Float -> Float) -> [[Complex Float]] -> [[Complex Float]]
zMatOp f mat = [map (zFloatOp f) xs | xs <- mat]

queryState :: Ket -> String
queryState k
        | k == z0 = "Up in Z"
        | k == z1 = "Down in Z"
        | k == x0 = "Up in X"
        | k == x1 = "Down in X"
        | k == y0 = "Up in Y"
        | k == y1 = "Down in Y"
        | otherwise = "Superposition with \945 = " ++ (show $ alpha k) ++ " and \946 = " ++ (show $ beta k)
