import Control.Monad
import Data.Maybe


main = do
    x <- getLine 
    let f = (last x)::Char
    if (f == 'a' || f=='A')
       then  putStrLn("Hola maca!")
    else do  putStrLn("Hola maco!")
    
   
