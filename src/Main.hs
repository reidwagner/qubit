module Main (main) where

import Qubit
import Data.Complex
import System.IO

--Test commit comment
--test commit comment 2

main = do 
	command <- prompt ">> " 
	handler command
	main

--loop :: IO()
--loop = putStr ": " >>= \_ -> getLine >>= handler >> loop

{-
--Output is buffered until a new line is printed. 
--This creates issues with same-line prompts. 
--Helper function to flush 
-}
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

handler :: String -> IO ()
handler s = do
        if s == "command" then putStrLn ("recognized ")  else putStrLn "unrecognized"


