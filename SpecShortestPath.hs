module SpecShortestPath
where
import Test.Hspec
import ShortestPath
import Data.Map (Map)
import qualified Data.Map as M
import Data.PSQueue (PSQ)
import Data.PSQueue as Q (empty, insert, fromList, toList, Binding (..), lookup)

main = hspec $ do
    describe "initialize" $ do
        it "fills the priority queue with infinity distances except for source" $ do 
            let g = M.fromList [(1,[(2,100),(3,150)]),(2,[(1,100),(3,20)]),(3,[(1,150),(2,20)])]
            let q = Q.fromList [1 :-> (infinity,1), 2 :-> (0,2), 3 :-> (infinity,3)]
            toList (initialize g 2) `shouldBe` (toList q)

    describe "settle" $ do
        it "remove the least distant node from the queue" $ do
            let g = M.fromList [(1,[(2,100),(3,150)]),(2,[(1,100),(3,20)]),(3,[(1,150),(2,20)])]
            let q = initialize g 2
            let (q',m) = settle (q,M.empty)
            Q.lookup 2 q' `shouldBe` Nothing
            
            
