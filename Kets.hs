module Kets
( kz0, kz1, kx0, kx1, ky0, ky1, gOp) where

import Data.Complex

c = 1/(sqrt 2)

kz0 = [[1 :+ 0],[0 :+ 0]] :: [[Complex Float]]
kz1 = [[0],[1]] :: [[Complex Float]]
kx0 = [[1],[1]] :: [[Complex Float]]
kx1 = [[1],[-1]] :: [[Complex Float]]
ky0 = [[1],[0 :+ 1]] :: [[Complex Float]]
ky1 = [[1],[0 :+ 1]] :: [[Complex Float]]

operate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
operate [[x],[y]] [[a,b],[c,d]] =  [[(x*a)+(y*b)], [(x*c)+(y*d)]]

gOp :: [[Complex Float]] -> String -> [[Complex Float]]
gOp xs "sgz" = operate xs [[1,0],[0,-1]]
gOp xs "sgx" = operate xs [[1,0],[0,-1]]
gOp xs "sgy" = operate xs [[1,0],[0,-1]]
gOp xs "had" = operate xs [[1,0],[0,-1]]
gOp xs "l" = operate xs [[1,0],[0,-1]]  
