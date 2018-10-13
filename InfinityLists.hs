
ones :: [Integer]
ones = 1:ones

fib :: [Integer]
fib = 0:1:zipWith (+) fib (tail fib)

nats:: [Integer]
nats = iterate (+1) 0


triangulars :: [Integer]
triangulars = 0:(scanl (+) 1 $ iterate (+1) 2)


ints :: [Integer]
ints = iterate (integer) 0
     where
          integer :: Integer -> Integer
          integer x
               | x > 0 = (-x)
               | otherwise = (-x) + 1


hasDivisors :: Integer -> Integer -> Bool
hasDivisors n c 
    | c * c > n     = False
    | mod n c == 0  = True
    | otherwise = hasDivisors n (c + 1)
    

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (hasDivisors n 2)

primes:: [Integer]
primes = filter (isPrime) (iterate (+1) 2) 

factorials :: [Integer]
factorials = scanl (*) 1 $ iterate (+1) 1 

hammings:: [Integer]
hammings =  1: merge (map(*2) hammings) (map(*3) hammings) (map(*5) hammings)

merge:: [Integer]-> [Integer]->[Integer]->[Integer]
merge x y z = merge' (merge' x y) z

merge':: [Integer] -> [Integer] -> [Integer]
merge' [] y = y
merge' x [] = x
merge' (x:xs) (y:ys)
    | x < y     = x: merge' xs (y:ys)
    | x > y    = y: merge' ys (x:xs)
    |otherwise  = merge' (x:xs) ys


tartaglia :: [[Integer]]
tartaglia = iterate next [1]

next::[Integer]->[Integer]
next x = zipWith (+) ([0] ++ x) (x ++ [0])

--aquest l he copiat: 
lookNsay :: [Integer]
lookNsay = iterate count 1

count :: Integer -> Integer
count a = read $ next' $ show a

next' :: [Char] -> [Char]
next' [] = []
next' cs = (show n) ++ [pr] ++ next' cua
  where 
    pr = head cs
    n = length $ takeWhile ( == pr) cs
    cua = dropWhile ( == pr) cs
