
ismultiple3or5::Integer->Bool
ismultiple3or5 n 
    | (mod n 3 ==0)     =True
    | (mod n 5 == 0)    = True
    | otherwise = False 

sumMultiples35 :: Integer -> Integer
sumMultiples35 n =  sum (filter (ismultiple3or5) [1..(n-1)])

fibonacci :: Int -> Integer
fibonacci n 
    | n == 0    = 0
    | n == 1    =1
    | otherwise = fibonacci(n-1) + fibonacci(n-2)



fib :: [Integer]
fib = 0:1:zipWith (+) fib (tail fib)


sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = sum (filter (even) (filter (<n) (take (fromInteger n) fib)))

-- largestPrimeFactor :: Int -> Int

-- isPalindromic :: Integer -> Bool