module Main (main) where

import Qubit
import Data.Complex

main = do 
	putStr ": "
	loop

loop :: IO()
loop = putStr ": " >>= \_ -> getLine >>= handler >> loop

handler :: String -> IO ()
handler s = do
        if s == "command" then putStrLn ("recognized ")  else putStrLn "unrecognized"


