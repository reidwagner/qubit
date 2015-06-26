module Kets
( kz0, kz1, kx0, kx1, ky0, ky1, gop, Gate) where

import Data.Complex

norm :: [[Complex Float]] -> [[Complex Float]]
norm vect = let sq2 = 1/(sqrt 2.0) :: Complex Float in [map (*sq2) xs | xs <- vect]

inner :: [[Complex Float]] -> [[Complex Float]] -> Complex Float
inner [[a],[b]] [[c],[d]] = a*c + b*d

--Kets
kz0 = [[1 :+ 0],[0 :+ 0]] :: [[Complex Float]]
kz1 = [[0],[1]] :: [[Complex Float]]
kx0 = norm [[1],[1]] :: [[Complex Float]]
kx1 = norm [[1],[-1]] :: [[Complex Float]]
ky0 = norm [[1],[0 :+ 1]] :: [[Complex Float]]
ky1 = norm [[1],[0 :+ 1]] :: [[Complex Float]]

operate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
operate [[x],[y]] [[a,b],[c,d]] =  [[(x*a)+(y*b)], [(x*c)+(y*d)]]

--Gate data type
data Gate = Sz | Sx | Sy | Had | L

--'g'ate 'op'eration
gop :: Gate -> [[Complex Float]] -> [[Complex Float]]
gop Sz xs = operate xs [[1,0],[0,-1]]
gop Sx xs = operate xs [[0,1],[1,0]]
gop Sy xs = operate xs [[0,0 :+ (-1)],[0 :+ 1,0]]
gop Had xs = norm $ operate xs [[1,1],[1,-1]]
gop L xs = operate xs [[1,0],[0,-1]]  

queryState :: [[Complex Float]] -> String
queryState k = "Spin " ++ dir k ++ " direction"
	where 	dir kz0 = "up in z"  --repeated patter. Doesn't work.
		dir kz1 = "down in z"
		dir kx0 = "up in x" 
		dir kx1 = "down in x"
		dir ky0 = "up in y"
		dir ky1 = "down in y"
queryState xs = "Superposition with c1 = " ++ (show $ (xs!!0)!!0) ++ " and c2 = " ++ (show $ (xs!!1)!!0) 
