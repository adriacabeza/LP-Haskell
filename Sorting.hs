insert :: [Int]-> Int-> [Int]
insert [] a = [] ++ [a]
insert (x:xs) a 
    | x >= a   =  [a] ++ [x] ++  xs
    | otherwise = [x] ++ insert xs a

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x


remove :: [Int]->Int->[Int]
remove [] a = []
remove (x:xs) a 
    | x == a    = xs
    | otherwise     = [x] ++ remove xs a 
    

myMinimum :: [Int]-> Int
myMinimum [x] = x
myMinimum (x:xs)
    | x < m = x
    | otherwise = m
    where m = myMinimum xs
          
          
ssort:: [Int] -> [Int]
ssort [] = []
ssort [x] = [x]
ssort x = m:ssort(remove x m)
    where m = myMinimum x


merge:: [Int]-> [Int]->[Int]
merge [] [] = []
merge [] b = b
merge a [] = a
merge (x:xs) (y:ys) 
    |(x <= y)  = x:(merge xs (y:ys)) 
    |otherwise = y:(merge (x:xs) ys)


msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
-- take i drop 
msort x =  merge (msort a) (msort b)
    where 
         a = take (div (length x) 2) x
         b = drop(div (length x) 2) x
  
qsort::[Int]->[Int]
qsort [] = []
qsort [x] = [x]
qsort (x:xs)  = qsort small ++[x]++ qsort large
    where 
        small = [y | y<-xs, y<=x]
        large = [y | y<-xs, y>x]
        
        
genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort [x] = [x]
genQsort (x:xs)  = genQsort small ++[x]++ genQsort large
    where 
        small = [y | y<-xs, y<=x]
        large = [y | y<-xs, y>x]
        
-- quicksort (x:xs) = (quicksort lesser) ++[x] ++ (quicksort greater)
--     where lesser  = filter (<x) xs
--         greater = filter (>=x) xs
        
    
    

                        
