
import System.IO


calcIMC = do name <- getWord
             if name == "*" then return()
               else do
               w <- getWord
               h <- getWord
               putStr name
               putStr ": "
               putStrLn $ message $ bmi (read w :: Float) (read h :: Float)
               calcIMC

message::Float->String
message bmi
        |  (bmi < 18)    ="underweight"
        |  (bmi >= 18) && (bmi < 25)    ="normal weight"
        |  (bmi >= 25) && (bmi <30)    ="overweight"
        |  (bmi >= 30) && (bmi < 40)    ="obese"
        | otherwise   = "severely obese"
   

bmi:: Float -> Float -> Float
bmi w h = w / (h * h)

--getWord :: IO String 
getWord = do c <- getChar
             if (c == '\n') || (c == ' ')
               then return ""
               else do w <- getWord
                       return (c:w)

                  
main = calcIMC