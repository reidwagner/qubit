import Kets
import Data.Complex

main = do
	putStrLn (super kz0)
	putStrLn "Applying Hadamard gate.."
	putStrLn (super (gop Had kz0))
