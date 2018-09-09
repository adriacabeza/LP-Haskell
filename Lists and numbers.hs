absValue :: Int -> Int
absValue n 
    | n < 0 = -n
    | otherwise = n

power :: Int -> Int -> Int
power n m  
    | n  == 0 = 0
    | m == 0 = 1
    | otherwise = power n (m-1) * n
    

prod :: Int-> Int -> Int 
prod n m
    | n == 0 = 0
    | y == 0 = 2*(prod x m)
    | otherwise = (prod (n-1) m) + m
    where x = div n 2
          y = mod n 2

hasDivisors :: Int -> Int -> Bool
hasDivisors n c 
    | c * c > n     = False
    | mod n c == 0  = True
    | otherwise = hasDivisors n (c + 1)
    

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (hasDivisors n 2)


myLength :: [Int]-> Int
myLength [] = 0 
myLength (x:xs) =  1 + myLength xs

myMaximum :: [Int]-> Int
myMaximum [x] = x
myMaximum (x:xs)
    | x > m = x
    | otherwise = m
    where m = myMaximum xs

    
average :: [Int] -> Float
average x = (fromIntegral $ suma x) /  (fromIntegral $ myLength x)

    
suma :: [Int] -> Int
suma [] = 0 
suma (x:xs) = x + suma xs 
 

girallista :: [Int] -> [Int]
girallista [x] = [x]
girallista(x:xs) =  girallista xs ++ [x] 

buildPalindrome :: [Int] -> [Int]
buildPalindrome x = girallista x ++ x
 
slowFib :: Int -> Int 
slowFib n
    | n == 0    = 0
    | n == 1    =1
    | otherwise = slowFib(n-1) + slowFib(n-2)



quickFib :: Integer -> Integer
quickFib n = fst (quickFib' n)

quickFib' :: Integer -> (Integer, Integer)
quickFib' 0 = (0, 1)
quickFib' 1 = (1, 1)
quickFib' n = (seg, seg + res)
    where (res, seg) = quickFib' (n-1)



oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens [] = ([], [])
oddsNevens (x:xs)
    |mod x 2 == 1   = (x:a,b)
    |otherwise      = (a,x:b) 
    where (a,b) = oddsNevens xs

-- primeDivisors :: Int -> [Int]
-- primeDivisors 1 = []
-- primeDivisors n
--     |isPrime n == True  = [n]
--     |


aux_remove :: [Int] -> Int ->[Int]
aux_remove (x:xs) y
    | x == y    = aux_remove xs y
    | otherwise     = x:aux_remove xs y
aux_remove [] y = []


remove :: [Int] -> [Int] -> [Int]
remove x [] = x
remove [] y = []
remove x (y:ys)= remove (aux_remove x y) ys

flatten :: [[Int]] -> [Int] 
flatten [] = []
flatten (x:xs) = x ++ flatten(xs)


