module SpecShortestPath
where
import Test.Hspec
import ShortestPath
import Data.Map (Map)
import qualified Data.Map as M (empty, toList, fromList)
import Data.PSQueue (PSQ)
import Data.PSQueue as Q (empty, insert, fromList, toList, Binding (..), lookup, size)

main = hspec $ do
    describe "initialize" $ do
        it "fills the priority queue with infinity distances except for source" $ do 
            let g = M.fromList [(1,[(2,100),(3,150)]),(2,[(1,100),(3,20)]),(3,[(1,150),(2,20)])]
            let q = Q.fromList [1 :-> (infinity,1), 2 :-> (0,2), 3 :-> (infinity,3)]

            toList (initialize g 2) `shouldBe` (toList q)

    describe "settle" $ do
        it "remove the least distant node from the queue and insert it in the path" $ do
            let g = M.fromList [(1,[(2,100),(3,150)]),(2,[(1,100),(3,20)]),(3,[(1,150),(2,20)])]
            let q = initialize g 2

            let (q',p) = settle (q,M.empty)
            Q.lookup 2 q' `shouldBe` Nothing
            Q.size q'     `shouldBe` 2
            M.toList p    `shouldBe` [(2,(0,2))]

    describe "neighbors" $ do
        it "calculate the distances from closest node to its neighbors" $ do
            let g = M.fromList [(1,[(2,100),(3,150)]),(2,[(1,100),(3,20)]),(3,[(1,150),(2,20)])]
            let q = initialize g 2

            let n = neighbors 2 4807 g
            n `shouldBe` [(1,4907),(3,4827)]

    describe "adjust" $ do
        it "adjust the distances in the queue if shorter distances are found" $ do
            let g = M.fromList [(1,[(2,100),(3,150)]),(2,[(1,100),(3,20)]),(3,[(1,150),(2,20)])]
            let q = initialize g 2
            let n = [(1,100),(2,4807),(3,20)]
 
            let q' = adjust 2 n q
            let r = Q.fromList [1 :-> (100,2), 2 :-> (0,2), 3 :-> (20,2)]
            toList q' `shouldBe` (toList r)

    describe "shortestPath" $ do
        it "find the shortest path for the neighbors of the closest nodes to source" $ do
            let g = M.fromList [(1,[(2,100),(3,150)]),(2,[(1,100),(3,20)]),(3,[(1,150),(2,20)])]
            let q = initialize g 1
            let p = M.empty
            let (q',p') = shortestPath g q p
            
            M.toList p' `shouldBe` [(1,(0,1))]
            let r = Q.fromList [2 :-> (100,1), 3 :-> (150,1)]
            toList q' `shouldBe` (toList r)
