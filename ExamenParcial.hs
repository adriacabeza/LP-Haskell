multEq :: Int -> Int -> [Int]
multEq x y = iterate (\a-> a*x*y) 1

selectFirst :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst  [] _ _ = []
selectFirst  _ [] _ = []
selectFirst (x:xs) y z 
    | (get_ith 0 x y) == -1  = selectFirst xs y z
    | (get_ith 0 x z) == -1  = x:(selectFirst xs y z)
    | ((get_ith 0 x y) < (get_ith 0 x z))   =x:(selectFirst xs y z)
    | otherwise =selectFirst xs y z
    
get_ith :: Int-> Int -> [Int]-> Int
get_ith i x [] = (-1)
get_ith i x l 
    | (elem x l) && ((l !! i) == x)   =i
    | i == (length l)   = (-1)
    | otherwise =get_ith (i+1) x l 
    

type SymTab a = String -> Maybe a
 
empty :: SymTab a 
empty = (\ _ -> Nothing)  
-- empty = const Nothing
 
 
get :: SymTab a -> String -> Maybe a 
get a s = a s
-- get = ($)
 
 
set :: SymTab a -> String -> a -> SymTab a  
set st k v x 
    | k == x =Just v  
    | otherwise =st x

-- myIterate :: (a -> a) -> a -> [a]
