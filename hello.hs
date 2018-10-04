import Control.Monad
import Data.Maybe


main = do
    x <- getLine 
    let f = (last x)::Char
    if (f == 'a' || f=='A')
       then  putStrLn("Hola maca!")
    else do  putStrLn("Hola maco!")
    
    

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
 
eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add a b) = (eval1 a) + (eval1 b)
eval1 (Sub a b) = (eval1 a) - (eval1 b)
eval1 (Mul a b) = (eval1 a) * (eval1 b)
eval1 (Div a b) = div (eval1 a) (eval1 b) 


{-
eval2 (Add a b) = do 
    n1 <- eval2 a
    n2 <- eval2 b
    return $ n1 + n2 

eval2 (Sub a b) = eval2 a >>= \x -> eval2 b >>= \y -> return $ x - y 

   -} 
    

eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just x

eval2 (Add a b) = do 
    let n1 = eval2 a
    let n2 = eval2 b
    if n1 == Nothing || n2 == Nothing then Nothing
                                      else do Just(fromJust(n1)+fromJust(n2))
eval2 (Sub a b) = do
    let n1 = eval2 a
    let n2 = eval2 b
    if n1 == Nothing || n2 == Nothing then Nothing
                                      else do Just(fromJust(n1)-fromJust(n2))                                                                    
eval2 (Mul a b) = do
    let n1 = eval2 a
    let n2 = eval2 b
    if n1 == Nothing || n2 == Nothing then Nothing
                                      else do Just(fromJust(n1)*fromJust(n2))
eval2 (Div a b) = do
    if eval2 b == (Just 0)
        then Nothing
    else do Just(div (fromJust(eval2 a)) (fromJust(eval2 b)))
    
    
eval3 :: Expr -> Either String Int 
eval3 (Val x) = Right x
eval3 (Add a b) = do 
    m <- eval3 a
    n <- eval3 b
    Right(n+m)
eval3 (Sub a b) = do 
    m <- eval3 a
    n <- eval3 b
    Right(m-n)
eval3 (Mul a b) = do 
    m <- eval3 a
    n <- eval3 b
    Right(n*m)
eval3 (Div a b) = do 
    m <- eval3 a
    n <- eval3 b
    if(n == 0) 
       then Left("div0")
       else do Right(div n m)
