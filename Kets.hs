module Kets
( kz0, kz1, kx0, kx1, ky0, ky1, gOp, Gate) where

import Data.Complex

norm :: [[Complex Float]] -> [[Complex Float]]
norm vect = let sq2 = 1/(sqrt 2.0) :: Complex Float in [map (*sq2) xs | xs <- vect]

kz0 = [[1 :+ 0],[0 :+ 0]] :: [[Complex Float]]
kz1 = [[0],[1]] :: [[Complex Float]]
kx0 = [[1],[1]] :: [[Complex Float]]
kx1 = [[1],[-1]] :: [[Complex Float]]
ky0 = [[1],[0 :+ 1]] :: [[Complex Float]]
ky1 = [[1],[0 :+ 1]] :: [[Complex Float]]

operate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
operate [[x],[y]] [[a,b],[c,d]] =  [[(x*a)+(y*b)], [(x*c)+(y*d)]]

data Gate = Sz | Sx | Sy | Had | L

gOp :: Gate -> [[Complex Float]] -> [[Complex Float]]
gOp Sz xs = operate xs [[1,0],[0,-1]]
gOp Sx xs = operate xs [[0,1],[1,0]]
gOp Sy xs = operate xs [[0,0 :+ (-1)],[0 :+ 1,0]]
gOp Had xs = norm $ operate xs [[1,1],[1,-1]]
gOp L xs = operate xs [[1,0],[0,-1]]  
