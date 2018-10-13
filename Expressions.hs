
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
eval2 (Val x) = return x

eval2 (Add a b) = do 
    n1 <- eval2 a
    n2 <- eval2 b
    return $ n1 + n2 

eval2 (Sub a b) = do
    n1 <- eval2 a
    n2 <- eval2 b
    return $ n1 - n2 
    
    
eval2 (Mul a b) = do
    n1 <- eval2 a
    n2 <- eval2 b
    return $ n1 * n2 


eval2 (Div a b) = do
    n1 <- eval2 a
    n2 <- eval2 b
    if n2 == 0
        then Nothing
    else do return $ div n1 n2
    
    
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
       else do Right(div m n)

