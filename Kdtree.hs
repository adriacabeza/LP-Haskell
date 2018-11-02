module Kdtree where    
import Control.Applicative
import Control.Monad

--author: Adrià Cabeza 

--Exercici1 

class Point p where 
    
    sel :: Int -> p -> Double
    dim:: p -> Int
    child:: p-> p-> [Int]->Int
    dist:: p->  p -> Double
    list2Point:: [Double]-> p 
    comp:: p -> p -> Int -> Int
    ptrans::[Double]->p -> p
    prova::p-> Bool


--Exercici2

data Point3d = Point3d [Double]
    deriving (Eq)

instance Point Point3d where

    sel coord (Point3d p1) = p1 !! (coord-1)

    dim _ = 3

    comp n1 n2 x
        | (sel (x) n1) >= (sel (x) n2)  =0
        | otherwise =1

    child e1 e2 l = binarytoNum  $ map (comp e1 e2) l
            
    dist (Point3d e1) (Point3d e2) = sqrt(sum (map (^2) (zipWith (-) e1 e2) ))
    
    list2Point l = Point3d l

    ptrans x (Point3d p)  = Point3d (zipWith (+) p x)

    prova (Point3d x)
        |(x !! 0)>1.5  = True
        |otherwise     =False


        
instance Show Point3d where
    show ( Point3d (x:xs) ) = "(" ++ (show x) ++ (concatMap(\x -> "," ++ (show x)) xs)  ++ ")"
 -- show (Point3d x) =   "(" ++ init((concatMap (\x -> (show x) ++ ",") x)) ++ ")"



--Exercici3 
   
data Kd2nTree a = Node a [Int] [Kd2nTree a] | Empty

instance (Point a, Eq a) => Eq (Kd2nTree a) where
    --si que has de mirar que sigui el mateix conjunt 
    Empty == Empty  =True
    n == m = length(get_all n) == length(get_all m) && all (contains n) (map (fst) (get_all m))

instance Show a => Show (Kd2nTree a) where
    show Empty = ""
    show (Node a l f) = show(a) ++ show(l) ++ "\n"++ (show2 f 0 0)  

   
show2:: Show a => [Kd2nTree a] -> Int -> Int -> String
show2 [] _  _= ""
show2 (Empty:as) x tabs= (show2 as (x+1) tabs)
show2 ((Node a l f):as) x tabs= " " ++ concat(replicate tabs "     ")  ++ "<" ++ (show x) ++ "> "  
                                ++ (show a) ++ " " ++ (show l)  ++ "\n"
                                ++ (show2 f 0 (tabs+1))  ++ (show2 as (x+1) tabs)



--Exercici 4 

insert:: Point p=> Kd2nTree p ->  p -> [Int] -> Kd2nTree p
insert Empty p l = Node p l (replicate (2^ length l) Empty)
insert (Node a l []) p l2 = Node a l [(insert Empty p l2)]
insert (Node a l f) p l2 = Node a l (m ++ 
                                    [(insert (last t) p l2)]
                                    ++ (drop (n+1) f))
    where    n = (child a p l)
             m = take n f
             t = take (n+1) f

build:: Point p => [(p,[Int])] ->Kd2nTree p
build [] = Empty
build [(p,l)] = Node p l (replicate (2^ length l) Empty)
build (x:xs) = insert' (build [x]) xs 

insert':: Point p => Kd2nTree p -> [(p,[Int])]-> Kd2nTree p
insert' t [] = t
insert' t ((p,l1):ls) =  insert' (insert t p l1) ls

buildIni :: Point p =>[([Double],[Int])]-> Kd2nTree p
buildIni [] = Empty
buildIni x =build (zip (map (list2Point) (map (fst) x))  (map (snd) x)  )


--Exercici5


get_all:: Point p => Kd2nTree p -> [(p,[Int])]
get_all Empty = []
get_all (Node a l []) = [(a,l)]
get_all (Node a l (f:fs)) = get_all(f) ++ get_all(Node a l fs)


--Exercici6

remove :: (Point p,Eq p) => Kd2nTree p ->p -> Kd2nTree p 
remove Empty _ = Empty
remove (Node a l f) p 
    | (a==p)    =  build (concatMap (get_all) f)
    | not (contains (l1) p)  = (Node a l f)
    | otherwise = Node a l ((take (fill) f) ++ [remove l1 p] ++ (drop (fill+1) f))
    where 
        fill = child a p l
        l1= head (drop fill f)


--Exercici7

contains:: (Eq p, Point p) => Kd2nTree p-> p -> Bool
contains Empty _ = False
contains (Node a l f) p = (a==p) || contains (f !! (child a p l)) p
    


--Exercici8

nearest ::(Point p) => Kd2nTree p -> p -> p
nearest t p =  minim (tots) (tots !! 0) p
        where 
            tots =  map (fst) (get_all t)
            minim :: (Point p) => [p] -> p -> p -> p
            minim [] min _ = min
            minim (t:ts) min punt
                | (dist punt t) < (dist punt min)  = minim ts t punt
                | otherwise = minim ts min punt

        
--Exercici9 

allinInterval:: Point p => Kd2nTree p -> p -> p -> [p]
allinInterval t pmin pmax =  filter (interval pmin pmax) (map (fst) (get_all t))
        where 
        interval:: Point p => p -> p -> p-> Bool
        interval pmin pmax punt = (compara pmin punt 1) && (compara punt pmax 1)
                


--Exercici10

instance Functor Kd2nTree where
    fmap f Empty = Empty 
    fmap f (Node a l h) = (Node (f a) l (map (fmap f) h))

translation:: Point t => [Double] -> Kd2nTree t -> Kd2nTree t
translation l a = (fmap (ptrans l) a )  



--Exercici11

instance Applicative Kd2nTree where
    pure = return
    (<*>) = ap 


instance Monad Kd2nTree where 
    return a = Node a [] []
    Empty >>= f = Empty
    (Node p l fills) >>= f  = f p

kfilter::Point p =>(p->Bool)-> Kd2nTree p -> Kd2nTree p
kfilter f Empty = Empty
kfilter f (Node r l ps)= if (f r) then Node r l (do 
                                                (Node r1 l1 ps1) <- ps
                                                if (f r1) then [Node r1 l1 [kfilter f c | c<-ps1]]
                                                else [])
                        else foldl (unio) Empty (map (kfilter f) ps)


-- OPCIÓ 2, no fa servir mònades
-- kfilter f Empty = Empty 
-- kfilter f (Node a l h) = 
--          if(f a) 
--                 then foldl (unio) (insert Empty a l) (map (kfilter f) h)
--          else do foldl (unio) Empty (map (kfilter f) h)

--funció de prova per veure si funciona la instància mònade
kfilterNode :: (b -> Bool) -> Kd2nTree b -> Kd2nTree b
kfilterNode fb kt@(Node n l h) = do
    p <- kt
    if fb p then Node n l []
    else Empty       

unio:: Point p=> Kd2nTree p -> Kd2nTree p -> Kd2nTree p
-- unio t1 t2 =  build ((get_all t1) ++ (get_all t2))
unio t1 t2 =  insert' t1 (get_all t2) 



--AUXILIARS

-- compara lexicogràficament les coordenades d'un punt i si el primer és més petit que el segon dóna TRUE
compara :: Point p=> p -> p -> Int -> Bool
compara e1 e2 n 
    | n == (dim e1) = False
    | (sel n e1) < (sel n e2) =True
    | (sel n e1) == (sel n e2) = (compara e1 e2 (n+1))
    | otherwise = False 


--auxiliar per a fer un primer test de si unió funciona
size::Point p => Kd2nTree p -> Int
size Empty = 0
size t =  length(get_all t)


--funció auxiliar que converteix una llista de 0 i 1 (representació binària) al seu valor enter
binarytoNum:: [Int] -> Int
binarytoNum [x] = x 
binarytoNum (x:xs) = x * 2^((length xs)) + binarytoNum xs

--ARBRES DE PROVA

exampleSet :: Kd2nTree Point3d 
exampleSet = build [(Point3d[3.0,-1.0,2.1],[1,3]),(Point3d[3.5,2.8,3.1],[1,2]),(Point3d[3.5,0.0,2.1],[3]),(Point3d[3.0,-1.7,3.1],[1,2,3]), (Point3d[3.0,5.1,0.0],[2]),(Point3d[1.5,8.0,1.5],[1]),(Point3d[3.3,2.8,2.5],[3]),(Point3d[4.0,5.1,3.8],[2]), (Point3d[3.1,3.8,4.8],[1,3]),(Point3d[1.8,1.1,-2.0],[1,2])]

arbrebuit :: Kd2nTree Point3d
arbrebuit = Empty

exampleSet2 :: Kd2nTree Point3d
exampleSet2 = buildIni [([3.0,-1.0,2.1],[1,3]),([3.5,2.8,3.1],[1,2]),([3.5,0.0,2.1],[3]),([3.0,-1.7,3.1],[1,2,3]), ([3.0,5.1,0.0],[2]),([1.5,8.0,1.5],[1]),([3.3,2.8,2.5],[3]),([4.0,5.1,3.8],[2]), ([3.1,3.8,4.8],[1,3]),([1.8,1.1,-2.0],[1,2])] 

