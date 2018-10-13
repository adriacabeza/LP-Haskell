data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int 
size (Empty) = 0
size (Node a l r) = size l + size r + 1

height :: Tree a -> Int
height Empty = 0
height (Node a l r) = 1 + max (height l) (height r)

equal :: Eq a => Tree a -> Tree a -> Bool 
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node a l r) (Node b lb rb) = a == b && equal  l lb && equal  r rb


isomorphic :: Eq a => Tree a -> Tree a -> Bool 
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node a l r) (Node b lb rb) = a == b && ((isomorphic l lb && isomorphic r rb) || (isomorphic l rb && isomorphic r lb))

preOrder :: Tree a -> [a] 
preOrder Empty = []
preOrder (Node a l r) = a:(preOrder l ++ preOrder r)

postOrder :: Tree a -> [a]
postOrder Empty =[]
postOrder (Node a l r)= (postOrder l ++ postOrder r) ++ [a]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a l r)= (inOrder l) ++[a]++ (inOrder r)


--breadthFirst :: Tree a -> [a]

-- build :: Eq a => [a] -> [a] -> Tree a

-- overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
