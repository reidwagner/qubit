module Qubit
( sz, sx, sy, had, l
, z0, z1, x0, x1, y0, y1
, super, operate) 
where

import Data.Complex
import Data.List

--All operations are done with complex float matrices
type Ket = [[Complex Float]] 
type Bra = [[Complex Float]] -- should be [Complex Float] ?
type Operator = [[Complex Float]]

{- 
 - Kets representing spin in x,y,z directions. 
 -}
z0 = [[1 :+ 0],[0 :+ 0]] :: Ket -- <- Basis
z1 = [[0],[1]] :: Ket		-- <- 
x0 = norm [[1],[1]] :: Ket
x1 = norm [[1],[-1]] :: Ket
y0 = norm [[1],[0 :+ 1]] :: Ket
y1 = norm [[1],[0 :+ 1]] :: Ket

{-
 - Following are some basic quantum gate operators
 -}
sz = [[1,0],[0,-1]] :: Operator 		-- <- Pauli
sx = [[0,1],[1,0]] :: Operator			-- <-
sy = [[0,0 :+ (-1)],[0 :+ 1,0]] :: Operator     -- <-
had = norm [[1,1],[1,-1]] :: Operator 		-- <- Hadamard
l = [[1,0],[0,-1]] :: Operator                  

{-
 - Following are a series of functions for calculations.
 - Includes some quantum operations as well as simple math.  
 -}

-- Tensor product. (x)  
tensorProd :: Ket -> Ket -> [[Complex Float]] 
tensorProd k0 k1 = let combined = [(*)] <*> concat k0 <*> concat k1 in [take len combined, drop len combined]
	where len = length k0

-- Measures a quantum state. Returns of a tuple of measurement probability in each basis vector. 
measure :: Ket -> (Complex Float, Complex Float)
measure k = ((alpha k)^2, (beta k)^2) 

-- Inner product.
inner :: Bra -> Ket -> Complex Float
inner [[a],[b]] [[c],[d]] = a*c + b*d

-- Operation of operator on ket. Will need to be generalized.
operate :: Ket -> Operator -> Ket
operate [[x],[y]] [[a,b],[c,d]] =  [[(x*a)+(y*b)], [(x*c)+(y*d)]]

{-
 - alpha - returns first coefficient
 - beta - returns second coefficient 
 -}
alpha :: Ket -> Complex Float
alpha k = k!!0!!0

beta :: Ket -> Complex Float
beta k = k!!1!!0

--Simply normalizes the vector [1,1]. Will need to be generalized. 
norm :: [[Complex Float]] -> [[Complex Float]]
norm z = let sq2 = 1/(sqrt 2.0) :: Float in zMatOp (*sq2) z -- [map (*sq2) xs | xs <- vect]

-- Round to hundredth.  
roundDec :: Float -> Float
roundDec f = let c = 1000.0 :: Float in (fromInteger $ round $ f * (1000))/c

-- Complex float operation. Carries out float operation on each component.  
zFloatOp :: (Float -> Float) -> Complex Float -> Complex Float
zFloatOp f z = ((f $ realPart z) :+ (f $ imagPart z))

-- Applies function to each element of matrix.   
zMatOp :: (Float -> Float) -> [[Complex Float]] -> [[Complex Float]]
zMatOp f mat = [map (zFloatOp f) xs | xs <- mat]

{-
 - Following are functions for producing string output regarding calculations. 
 -}

-- Creates readable string of the form c0|0> + c1|1>, with a few specific cases. 
-- E.g. |1> for spin up. 
super :: Ket -> String
super k 
	| alphak == "" = betak
	| betak == "" = alphak
	| otherwise = alphak ++ " + " ++ betak
	where 	alphak = outmod (readComplex $ alpha k) "|0>" 
		betak = outmod (readComplex $ beta k) "|1>"

-- Modifies strings to look nicer. E.g. 1.0|1> -> |1>, 0.0|0> -> ""
outmod :: String -> String -> String
outmod s k  
	| s == "1.0" = k
	| s == "0" = ""
	| s == "1.0i" = "i" ++ k
	| otherwise = s ++ k

-- Creates string displaying row values
rowToString :: [Complex Float] -> String
rowToString row = foldr (\acc x -> acc ++ " " ++ x) "" (map readComplex row) 

-- Creates string representing matrix
matToString :: [[Complex Float]] -> String 
matToString mat = foldr (\acc x -> acc ++ "\n" ++ x) "" (map rowToString mat)

--Read complex float. 
readComplex :: Complex Float -> String
readComplex z 
	| all (== 0) [real, imag] = "0" 
	| all (/= 0) [real, imag] = "(" ++ (show $ real) ++ " + " ++ (show $ imag) ++ "i)" 
	| real /= 0 = (show $ real) 
	| imag /= 0 = (show $ imag) ++ "i"
 	where 	real = roundDec $ realPart z
		imag = roundDec $ imagPart z

--Provides description of quantum state. 
queryState :: Ket -> String
queryState k
        | k == z0 = "Up in Z"
        | k == z1 = "Down in Z"
        | k == x0 = "Up in X"
        | k == x1 = "Down in X"
        | k == y0 = "Up in Y"
        | k == y1 = "Down in Y"
        | otherwise = "Superposition with \945 = " ++ (show $ alpha k) ++ " and \946 = " ++ (show $ beta k)
