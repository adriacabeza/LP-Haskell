eql :: [Int] -> [Int] -> Bool 
-- eql listA listB =  listA == listB
eql a b = (length(a) == length(b))  && (and (zipWith (==) a b))

prod :: [Int] -> Int 
-- prod x = (product x)
prod x = foldl (*) 1 x 

prodOfEvens :: [Int] -> Int 
prodOfEvens x = foldl (*) 1 (filter even x)

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1
    
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct x y = sum $ zipWith (*) x y
