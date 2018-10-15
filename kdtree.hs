
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
    show (Point3d x) =   "(" ++ init((concatMap (\x -> (show x) ++ ",") x)) ++ ")"

   

  --funció auxiliar
binarytoNum:: [Int] -> Int
binarytoNum [x] = x 
binarytoNum (x:xs) = x * 2^((length xs)) + binarytoNum xs




-- -- per crear un kd 2^n Tree ens donaran una llista de parells 
-- --que contenen dues llistes, la primera són les coordenades
-- -- i la segona és la llista de coordenades que s'usarà per 
-- -- distribuir els següents punts entre els seus fills
-- data Kd2nTree =  [  ( [Double] , [Double] )  ]
-- -- en algunes operacions ens caldrà que aquest tipus
-- -- sigui de la classe Point o de la Show



