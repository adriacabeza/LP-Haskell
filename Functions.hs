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

flatten :: [[Int]] -> [Int] 
flatten x = foldl (++) [] x

myLength :: String -> Int 
-- myLength x = length x
myLength l = foldl (+) 0 (map(const 1) l) 
-- myLength a = foldl (\y x -> y + 1) 0 a

myReverse :: [Int] -> [Int] 
myReverse x = foldl (flip (:)) [] x
-- myReverse a = foldr (\x y -> y++[x]) [] a

countIn :: [[Int]] -> Int -> [Int] 
countIn l x = map (length) $ map (filter (== x)) l
    

firstWord :: String -> String 
firstWord text = takeWhile (/= ' ') $ dropWhile (== ' ') text

ones :: [Integer]
ones = 1:ones

fib :: [Integer]
fib = 0:1:zipWith (+) fib (tail fib)

