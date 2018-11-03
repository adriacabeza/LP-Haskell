module Test where
import Test.HUnit
import Kd2ntree
--author: Adrià Cabeza 

testsKdtree = TestList [TestLabel "insert" testInsert, TestLabel "remove" testRemove,TestLabel "translate" testTranslate, TestLabel "Nearest" testNearest, TestLabel "AllInInterval" testAllInterval,
                    TestLabel "Contains" testContains, TestLabel "GetAll" testgetAll, TestLabel "Build d'ExampleSet" testBuild, TestLabel "insert" testInsert, TestLabel "BuildIni d'ExampleSet" testBuildIni] 

-- runTestTT testsKdtree -> to run KdTree tests 




-- remove :: (Point p,Eq p) => Kd2nTree p ->p -> Kd2nTree p 
expected1 = Node (Point3d[3.0,-1.0,2.1]) [1,3] [Node (Point3d[1.8,1.1,-2.0]) [1,2] [Empty,Node (Point3d[1.5,8.0,1.5]) [1] [Empty,Empty],Empty,Empty],Node (Point3d[3.0,-1.7,3.1]) [1,2,3] [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],Node (Point3d[3.5,0.0,2.1]) [3] [Empty,Empty],Node (Point3d[3.5,2.8,3.1]) [1,2] [Node (Point3d[3.3,2.8,2.5]) [3] [Empty,Empty],Node (Point3d[3.1,3.8,4.8]) [1,3] [Empty,Empty,Empty,Empty],Empty,Node (Point3d[4.0,5.1,3.8]) [2] [Empty,Empty]]]
testRemove = TestCase (assertEqual "Fem el remove d'un punt d'exampleSet" expected1 (remove exampleSet (Point3d [3.0,5.1,0.0])))



-- get_all:: Point p => Kd2nTree p -> [(p,[Int])]
expected2 = Node (Point3d[1.0,1.0,1.0]) [2,3] [Empty,Empty,Empty,Empty]
testInsert = TestCase (assertEqual "Fem insert d'un punt a Empty" expected2 (insert Empty (Point3d[1.0,1.0,1.0])[2,3]))


-- translation:: Point t => [Double] -> Kd2nTree t -> Kd2nTree t
expected3 = Node (Point3d[4.0,1.0,5.1]) [1,3] [Node (Point3d[4.0,7.1,3.0]) [2] [Node (Point3d[2.8,3.1,1.0]) [1,2] [Empty,Empty,Empty,Empty],Node (Point3d[2.5,10.0,4.5]) [1] [Empty,Empty]],Node (Point3d[4.0,0.30000000000000004,6.1]) [1,2,3] [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],Node (Point3d[4.5,2.0,5.1]) [3] [Empty,Empty],Node (Point3d[4.5,4.8,6.1]) [1,2] [Node (Point3d[4.3,4.8,5.5]) [3] [Empty,Empty],Node (Point3d[4.1,5.8,7.8]) [1,3] [Empty,Empty,Empty,Empty],Empty,Node (Point3d[5.0,7.1,6.8]) [2] [Empty,Empty]]]
testTranslate = TestCase (assertEqual "Fem translate de tots els punts" expected3 (translation [1,2,3] exampleSet))


-- nearest ::(Point p) => Kd2nTree p -> p -> p
expected5 = (Point3d[1.5,8.0,1.5])
testNearest = TestCase (assertEqual "Fem nearest del Point3d [1,24,3] amb exampleSet" expected5 (nearest exampleSet (Point3d [1,24,3])))

-- allinInterval:: Point p => Kd2nTree p -> p -> p -> [p]
expected6 = [(Point3d[3.0,5.1,0.0]),(Point3d[3.5,0.0,2.1]),(Point3d[3.3,2.8,2.5]),(Point3d[3.1,3.8,4.8]),(Point3d[3.5,2.8,3.1])]
testAllInterval = TestCase (assertEqual "Fem AllinInterval" expected6 ( allinInterval exampleSet (Point3d[3,1,1]) (Point3d[4,5,2]) ))


-- contains:: (Eq p, Point p) => Kd2nTree p-> p -> Bool
expected7 = False
testContains = TestCase (assertEqual "Fem contains del Point3d [1,1,1] a exampleSet" expected7 (contains exampleSet (Point3d [1,1,1])))


-- get_all:: Point p => Kd2nTree p -> [(p,[Int])]
input8 = exampleSet
expected8 = [((Point3d[1.8,1.1,-2.0]),[1,2]),((Point3d[1.5,8.0,1.5]),[1]),((Point3d[3.0,5.1,0.0]),[2]),((Point3d[3.0,-1.7,3.1]),[1,2,3]),((Point3d[3.5,0.0,2.1]),[3]),((Point3d[3.3,2.8,2.5]),[3]),((Point3d[3.1,3.8,4.8]),[1,3]),((Point3d[4.0,5.1,3.8]),[2]),((Point3d[3.5,2.8,3.1]),[1,2]),((Point3d[3.0,-1.0,2.1]),[1,3])]
testgetAll = TestCase (assertEqual "Fem un getAll d'exampleSet" expected8 (get_all input8))


-- build:: Point p => [(p,[Int])] ->Kd2nTree p
expected9 = Node (Point3d[3.0,-1.0,2.1]) [1,3] [Node (Point3d[3.0,5.1,0.0]) [2] [Node (Point3d[1.8,1.1,-2.0]) [1,2] [Empty,Empty,Empty,Empty],Node (Point3d[1.5,8.0,1.5]) [1] [Empty,Empty]],Node (Point3d[3.0,-1.7,3.1]) [1,2,3] [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],Node (Point3d[3.5,0.0,2.1]) [3] [Empty,Empty],Node (Point3d[3.5,2.8,3.1]) [1,2] [Node (Point3d[3.3,2.8,2.5]) [3] [Empty,Empty],Node (Point3d[3.1,3.8,4.8]) [1,3] [Empty,Empty,Empty,Empty],Empty,Node (Point3d[4.0,5.1,3.8]) [2] [Empty,Empty]]]
testBuild = TestCase (assertEqual "Build d'ExampleSet" expected9 (build ([(Point3d[3.0,-1.0,2.1],[1,3]),(Point3d[3.5,2.8,3.1],[1,2]),(Point3d[3.5,0.0,2.1],[3]),(Point3d[3.0,-1.7,3.1],[1,2,3]), (Point3d[3.0,5.1,0.0],[2]),(Point3d[1.5,8.0,1.5],[1]),(Point3d[3.3,2.8,2.5],[3]),(Point3d[4.0,5.1,3.8],[2]), (Point3d[3.1,3.8,4.8],[1,3]),(Point3d[1.8,1.1,-2.0],[1,2])])))


-- buildIni :: Point p =>[([Double],[Int])]-> Kd2nTree p
expected10 = Node (Point3d[3.0,-1.0,2.1]) [1,3] [Node (Point3d[3.0,5.1,0.0]) [2] [Node (Point3d[1.8,1.1,-2.0]) [1,2] [Empty,Empty,Empty,Empty],Node (Point3d[1.5,8.0,1.5]) [1] [Empty,Empty]],Node (Point3d[3.0,-1.7,3.1]) [1,2,3] [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],Node (Point3d[3.5,0.0,2.1]) [3] [Empty,Empty],Node (Point3d[3.5,2.8,3.1]) [1,2] [Node (Point3d[3.3,2.8,2.5]) [3] [Empty,Empty],Node (Point3d[3.1,3.8,4.8]) [1,3] [Empty,Empty,Empty,Empty],Empty,Node (Point3d[4.0,5.1,3.8]) [2] [Empty,Empty]]]
testBuildIni = TestCase (assertEqual "BuildIni d'ExampleSet" expected10 (buildIni [([3.0,-1.0,2.1],[1,3]),([3.5,2.8,3.1],[1,2]),([3.5,0.0,2.1],[3]),([3.0,-1.7,3.1],[1,2,3]), ([3.0,5.1,0.0],[2]),([1.5,8.0,1.5],[1]),([3.3,2.8,2.5],[3]),([4.0,5.1,3.8],[2]), ([3.1,3.8,4.8],[1,3]),([1.8,1.1,-2.0],[1,2])] ))


