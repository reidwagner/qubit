module Kets
( Gate(Sz, Sx, Sy, Had, L)
, kz0
, kz1
, kx0
, kx1
, ky0
, ky1
, gop
, super) 
where

import Data.Complex
import Data.List

data Gate = Sz | Sx | Sy | Had | L
type Ket = [[Complex Float]]
type Bra = [[Complex Float]]
type Operator = [[Complex Float]]

--Kets
kz0 = [[1 :+ 0],[0 :+ 0]] :: Ket
kz1 = [[0],[1]] :: Ket
kx0 = norm [[1],[1]] :: Ket
kx1 = norm [[1],[-1]] :: Ket
ky0 = norm [[1],[0 :+ 1]] :: Ket
ky1 = norm [[1],[0 :+ 1]] :: Ket

--'g'ate 'op'eration
gop :: Gate -> Ket -> Ket
gop Sz xs = operate xs [[1,0],[0,-1]]
gop Sx xs = operate xs [[0,1],[1,0]]
gop Sy xs = operate xs [[0,0 :+ (-1)],[0 :+ 1,0]]
gop Had xs = norm $ operate xs [[1,1],[1,-1]]
gop L xs = operate xs [[1,0],[0,-1]]

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
        | k == kz0 = "Up in Z"
        | k == kz1 = "Down in Z"
        | k == kx0 = "Up in X"
        | k == kx1 = "Down in X"
        | k == ky0 = "Up in Y"
        | k == ky1 = "Down in Y"
        | otherwise = "Superposition with \945 = " ++ (show $ alpha k) ++ " and \946 = " ++ (show $ beta k)
