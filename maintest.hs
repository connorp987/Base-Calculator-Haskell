import System.IO
import System.Environment   
import Data.List  

bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = 8 * bintodec (div i 10) + (mod i 10)
  
main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName  