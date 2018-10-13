
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f y [] = y
myFoldl f y (x:xs) = myFoldl f (f y x) xs


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f y [] = y
myFoldr f y (x:xs) = f x (myFoldr f y xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x: myIterate f (f x)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil f g y 
        | (f y) == True    = y
        | otherwise =myUntil f g (g y)     

--myMap :: (a -> b) -> [a] -> [b] 
-- myMap f [] = []
-- myMap f (x:xs) = f x : myMap f xs

myMap :: (a -> b) -> [a] -> [b] 
myMap f x = [ f l | l <- x]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f x = [ l | l <- x, f l ]
--myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter f [] = []
-- myFilter f (x:xs) 
--         | (f x) == True =x:myFilter f xs
--         | otherwise =myFilter f xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll f x = and (map f x)

--myAll :: (a -> Bool) -> [a] -> Bool
-- myAll f [] = True
-- myAll f (x:xs) 
--         | (f x) == True =myAll f xs
--         | otherwise = False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f x = or (map f x)
--myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f [] = False
-- myAny f (x:xs)
--         | (f x) == True     =True
--         | otherwise = myAny f xs

myZip :: [a] -> [b] -> [(a, b)]
myZip x [] = []
myZip [] y = []
myZip (x:xs) (y:ys) =  (x,y): myZip xs ys

--myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- myZipWith f x [] = []
-- myZipWith f [] y = []
-- myZipWith f (x:xs) (y:ys) = (f x y):myZipWith f xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f x y = [ f n m | (n,m)<-zip x y ]

countIf :: (Int -> Bool) -> [Int] -> Int 
countIf f x = length (filter f x)

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam x fs = [ map f x | f <-fs]
--pam l f = map (\fx -> map fx l) f

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 l f = map (\x -> map (\fx -> fx x) f) l



filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int 
filterFoldl f fx x l = foldl fx x (filter f l)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] 
insert _ [] x = [x]
insert f (x:xs) y 
    | f y x   =y:x:xs
    | otherwise = x:(insert f xs y)

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort _ [] = []
insertionSort f x = foldl (insert f) [] x
