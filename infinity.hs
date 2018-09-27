
ones :: [Integer]
ones = 1:ones

fib :: [Integer]
fib = 0:1:zipWith (+) fib (tail fib)

nats:: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = iterate (integer) 0
     where
          integer :: Integer -> Integer
          integer x
               | x > 0 = (-x)
               | otherwise = (-x) + 1


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



data Queue a = Queue [a] [a]
     deriving (Show)
 
create :: Queue a
create = Queue [] []
 
push :: a -> Queue a -> Queue a
push x (Queue b c) = Queue b (x:c)

pop :: Queue a -> Queue a
pop (Queue [] b) = pop Queue (reverse(b)++[]) []
pop (Queue (x:xs) c) = Queue xs c 

top :: Queue a -> a
-- top (Queue [] x) = last x
top (Queue (x:xs) _) = x  
  
empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue a b) = False 
 
--  arbres1,2,3,4

