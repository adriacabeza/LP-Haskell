
class Point p where 
-- donat un número que indica la coordenada i un element de tipus p retorna el valor de la coordenada 
    sel :: Int -> p -> Double

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


--feu que sigui instance d'aquesta classe, cosa que us obligarà
-- a definir les quatre funcions anteriors.
-- posteriorment haurem de fer que sigui de la classe Eq i de la
-- -- classe Show.
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

instance Show Point3d where
    --separar el x i l xs i fer que la com
    show (Point3d x) =   "(" ++ init((concatMap (\x -> (show x) ++ ",") x)) ++ ")"

   

  --funció auxiliar
binarytoNum:: [Int] -> Int
binarytoNum [x] = x 
binarytoNum (x:xs) = x * 2^((length xs)) + binarytoNum xs



data Kd2nTree a = Node a [Int] [Kd2nTree a] | Empty

--pensar pq l'enunciat diu que la igualtat estructural no val ja que dos conjunts són iguals si contenen els mateixos punts
-- instance Eq a => Eq (Kd2nTree a) where
    -- --si que has de mirar que sigui el mateix conjunt 
    -- Empty == Empty  =True
    -- _ == Empty  =False
    -- Empty == _  = False
    -- (Node x l f) == (Node y k g) = (x == y && l == k && f == g)

instance Show a => Show (Kd2nTree a) where
    show Empty = ""
    show (Node a l f) = show(a) ++ show(l) ++ "\n"++ (show2 f 0 0)  
    
show2:: Show a => [Kd2nTree a] -> Int -> Int -> String
show2 [] _  _= ""
show2 (Empty:as) x tabs= (show2 as (x+1) tabs)
show2 ((Node a l f):as) x tabs= concat(replicate tabs "    ")  ++ "<" ++ (show x) ++ "> "  ++ (show a) ++ " " ++ (show l)  ++ "\n"++ (show2 f 0 (tabs+1)) ++ (show2 as (x+1) tabs)


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


--mirar com passar els doubles del first a punts i passar al build
buildIni :: Point p =>[([Double],[Int])]-> Kd2nTree p
buildIni [] = Empty
buildIni [(x,y)] =build  (map (list2Point) x)  y


--en el cas que sigui empty m no funciona TODO: arreglar
-- contains:: (Eq p, Point p) => Kd2nTree p-> p -> Bool
-- contains Empty _ = False
-- contains (Node a l []) p = if (a == p) then True else False
-- contains (Node a l f) p 
--     | m == p    = True
--     | otherwise =(contains (f !! (child a p l)) p)
--     where (Node m t r) 
--         | (f !! (child a p l)) == Empty = Empty
--         | otherwise = f !! (child a p l)
     

exampleSet :: Kd2nTree Point3d 
exampleSet = build [(Point3d[3.0,-1.0,2.1],[1,3]),(Point3d[3.5,2.8,3.1],[1,2]),(Point3d[3.5,0.0,2.1],[3]),(Point3d[3.0,-1.7,3.1],[1,2,3]), (Point3d[3.0,5.1,0.0],[2]),(Point3d[1.5,8.0,1.5],[1]),(Point3d[3.3,2.8,2.5],[3]),(Point3d[4.0,5.1,3.8],[2]), (Point3d[3.1,3.8,4.8],[1,3]),(Point3d[1.8,1.1,-2.0],[1,2])]