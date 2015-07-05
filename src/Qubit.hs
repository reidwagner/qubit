{-# OPTIONS_GHC -fno-warn-tabs #-}

module Qubit
( gates
, kets
, super, operate, matToString) 
where

import Data.Complex
import Data.List

--All operations are done with complex float matrices
type State = [[Complex Float]] 
type Operator = [[Complex Float]]

{- 
 - States representing spin in x,y,z directions. 
 -}
z0 = [[1 :+ 0],[0 :+ 0]] :: State -- <- Basis
z1 = [[0],[1]] :: State		-- <- 
x0 = norm [[1],[1]] :: State
x1 = norm [[1],[-1]] :: State
y0 = norm [[1],[0 :+ 1]] :: State
y1 = norm [[1],[0 :+ 1]] :: State

kets = [z0,z1,x0,x1,y0,y1] :: [State]

{-
 - Following are some basic quantum gate operators
 -}
sz = [[1,0],[0,-1]] :: Operator 		-- <- Pauli
sx = [[0,1],[1,0]] :: Operator			-- <-
sy = [[0,0 :+ (-1)],[0 :+ 1,0]] :: Operator     -- <-
had = norm [[1,1],[1,-1]] :: Operator 		-- <- Hadamard
l = [[1,0],[0,-1]] :: Operator                  

gates = [sz,sx,sy,had,l] :: [Operator]

{-
 - Following are a series of functions for calculations.
 - Includes some quantum operations as well as simple math.  
 -}

-- Measures a quantum state. Returns of a tuple of measurement probability in each basis vector. 
measure :: State -> (Complex Float, Complex Float)
measure k = ((alpha k)^2, (beta k)^2) 

-- Inner product.
inner :: State -> State -> Complex Float
inner s1 s2 = sum $ zipWith (*) (head $ transpose s1) (head $ transpose s2)

--Should make instance of Num, Show
--add :: State -> State -> State

-- Operation of operator on ket. Will need to be generalized.
operate :: State -> Operator -> State
operate [[x],[y]] [[a,b],[c,d]] =  [[(x*a)+(y*b)], [(x*c)+(y*d)]]

operate :: State -> Operator -> State 
operate s o = 

{-
 - alpha - returns first coefficient
 - beta - returns second coefficient 
 -}
alpha :: State -> Complex Float
alpha k = k!!0!!0

beta :: State -> Complex Float
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
super :: State -> String
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
matToString mat = init $ init $ foldr (\acc x -> acc ++ "\n" ++ x) "" (map rowToString mat)

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
queryState :: State -> String
queryState k
        | k == z0 = "Up in Z"
        | k == z1 = "Down in Z"
        | k == x0 = "Up in X"
        | k == x1 = "Down in X"
        | k == y0 = "Up in Y"
        | k == y1 = "Down in Y"
        | otherwise = "Superposition with \945 = " ++ (show $ alpha k) ++ " and \946 = " ++ (show $ beta k)
