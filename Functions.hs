eql :: [Int] -> [Int] -> Bool 
eql listA listB =  listA == listB

prod :: [Int] -> Int 
prod x = (product x)

-- prodOfEvens :: [Int] -> Int 

-- -- fer servir la eveno per saber si es un numero parell

-- powersOf2 :: [Int]
    
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct x y  = sum $ zipWith (*) x y