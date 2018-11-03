-- exercici1
allsets :: a -> [[a]]
allsets n = iterate (n:) [] 
-- allsets n = []: map (n:) (allsets n)
-- allsets x = allsets' x []
--     where allsets' x l = l:(allsets' x (x:l))

--exercici2
-- alldivisors :: Int -> [[Int]]
-- alldivisors n = 


--exercici 3.1
data Expr a = Func String [Expr a] | Const a | Var String 


myFilter p l = do 
                x <- l
                if p x then
                 [x] ++ else []

                 