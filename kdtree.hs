
class Point p where 
    -- donat un número que indica la coordenada i un element de tipus p retorna el valor de la coordenada 
    sel :: Int -> p -> Double
    --TODO: mirar si aixo pot ser 0

    -- donat un element de tipus p retorna la seva dimensió
    dim:: p -> Int


    -- donat dos elements e1 i e2 de tipus p que representen punts
    -- i una llista de coordenades seleccionades retorna el número
    -- fill de e2 que li toca a e1
    child:: p-> p-> [Int]->Int

    -- donats dos elements e1 i e2 de tipus p ens retorna un Double que 
    -- és la distància entre e1 i e2
    dist:: p->  p -> Double

    --rep una llista de double i retorna un element de tipus p
    list2Point:: [Double]-> p 


    --compara dos coordenades de dos punts i si la segona és mes gran que la primera dóna 1 i sinó 0
    comp:: p -> p -> Int -> Int

    -- aplica una translació a un element de tipus p
    ptrans::[Double]->p -> p



data Point3d = Point3d [Double]
    deriving (Eq)

instance Point Point3d where

    sel coord (Point3d p1) = p1 !! coord
    
    dim _ = 3

    comp n1 n2 x
        | (sel (x-1) n1) >= (sel (x-1) n2)  =0
        | otherwise =1

    child e1 e2 l = binarytoNum  $ map (comp e1 e2) l
            
    dist (Point3d e1) (Point3d e2) = sqrt(sum (map (^2) (zipWith (-) e1 e2) ))
    
    list2Point l = Point3d l

    ptrans x (Point3d p)  = Point3d (zipWith (+) p x)

instance Show Point3d where
    show ( Point3d (x:xs) ) = "(" ++ (show x) ++ (concatMap(\x -> "," ++ (show x)) xs)  ++ ")"
 -- show (Point3d x) =   "(" ++ init((concatMap (\x -> (show x) ++ ",") x)) ++ ")"

   

--funció auxiliar que converteix una llista de 0 i 1 (representació binària) al seu valor enter
binarytoNum:: [Int] -> Int
binarytoNum [x] = x 
binarytoNum (x:xs) = x * 2^((length xs)) + binarytoNum xs



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
show2 ((Node a l f):as) x tabs= concat(replicate tabs "    ")  ++ "<" ++ (show x) ++ "> "  
                                ++ (show a) ++ " " ++ (show l)  ++ "\n"
                                ++ (show2 f 0 (tabs+1))  ++ (show2 as (x+1) tabs)


size::Point p => Kd2nTree p -> Int
size Empty = 0
size t =  length(get_all t)

insert:: Point p=> Kd2nTree p ->  p -> [Int] -> Kd2nTree p
insert Empty p l = Node p l (replicate (2^ length l) Empty)
insert (Node a l []) p l2 = Node a l [(insert Empty p l2)]
insert (Node a l f) p l2 = Node a l (m ++ 
                                    [(insert (last t) p l2)]
                                    ++ (drop (n+1) f))
    where    n = (child a p l)
             m = take n f
             t = take (n+1) f


get_all:: Point p => Kd2nTree p -> [(p,[Int])]
get_all Empty = []
get_all (Node a l []) = [(a,l)]
get_all (Node a l (f:fs)) = get_all(f) ++ get_all(Node a l fs)

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



contains:: (Eq p, Point p) => Kd2nTree p-> p -> Bool
contains Empty _ = False
contains (Node a l f) p = (a==p) || contains (f !! (child a p l)) p
    

exampleSet :: Kd2nTree Point3d 
exampleSet = build [(Point3d[3.0,-1.0,2.1],[1,3]),(Point3d[3.5,2.8,3.1],[1,2]),(Point3d[3.5,0.0,2.1],[3]),(Point3d[3.0,-1.7,3.1],[1,2,3]), (Point3d[3.0,5.1,0.0],[2]),(Point3d[1.5,8.0,1.5],[1]),(Point3d[3.3,2.8,2.5],[3]),(Point3d[4.0,5.1,3.8],[2]), (Point3d[3.1,3.8,4.8],[1,3]),(Point3d[1.8,1.1,-2.0],[1,2])]


exampleSet2 :: Kd2nTree Point3d
exampleSet2 = buildIni [([3.0,-1.0,2.1],[1,3]),([3.5,2.8,3.1],[1,2]),([3.5,0.0,2.1],[3]),([3.0,-1.7,3.1],[1,2,3]), ([3.0,5.1,0.0],[2]),([1.5,8.0,1.5],[1]),([3.3,2.8,2.5],[3]),([4.0,5.1,3.8],[2]), ([3.1,3.8,4.8],[1,3]),([1.8,1.1,-2.0],[1,2])] 

remove :: (Point p,Eq p) => Kd2nTree p ->p -> Kd2nTree p 
remove Empty _ = Empty
remove (Node a l f) p 
    | (a==p)    =  build (concatMap (get_all) f)
    | not (contains (l1) p)  = (Node a l f)
    | otherwise = Node a l ((take (fill) f) ++ [remove l1 p] ++ (drop (fill+1) f))
    where 
        fill = child a p l
        l1= head (drop fill f)


nearest ::(Point p) => Kd2nTree p -> p -> p
nearest t p =  minim (tots) (tots !! 0) p
        where 
            tots =  map (fst) (get_all t)
            minim :: (Point p) => [p] -> p -> p -> p
            minim [] min _ = min
            minim (t:ts) min punt
                | (dist punt t) < (dist punt min)  = minim ts t punt
                | otherwise = minim ts min punt

        
   


allinInterval:: Point p => Kd2nTree p -> p -> p -> [p]
allinInterval t pmin pmax =  filter (interval pmin pmax) (map (fst) (get_all t))
        where 
        interval:: Point p => p -> p -> p-> Bool
        interval pmin pmax punt = (comprova pmin punt 0) && (comprova punt pmax 0)
                

-- compara lexicogràficament les coordenades d'un punt i si el primer és més petit que el segon dóna TRUE
comprova :: Point p=> p -> p -> Int -> Bool
comprova e1 e2 n 
    | n == (dim e1) = False
    | (sel n e1) < (sel n e2) =True
    | (sel n e1) == (sel n e2) = (comprova e1 e2 (n+1))
    | otherwise = False 


instance Functor Kd2nTree where
    fmap f Empty = Empty 
    fmap f (Node a l h) = (Node (f a) l (map (fmap f) h))

translation:: Point t => [Double] -> Kd2nTree t -> Kd2nTree t
translation l a = (fmap (ptrans l) a )  


--instance Monad Kd2nTree where 
--kfilter::Point p=> (p->Bool)-> Kd2nTree p -> Kd2nTree p
--kfilter f Empty = Empty 
--kfilter f (Node a l t)
--	| f a	= build  ( [(a,f) ++ kfilter f t])
--	| not(f a) = build concatMap( (kfilter) t f)     
--idea 1 es fer funcio auxiliar passant f i passar tot el node i si el punt lo cumple també passes la seva llista 
-- gotta think a way to implement it with Monads
