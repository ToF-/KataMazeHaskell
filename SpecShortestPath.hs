module SpecShortestPath
where
import Test.Hspec
import ShortestPath
import Data.Map (Map)
import qualified Data.Map as M

main = hspec $ do
    describe "shortest paths function" $ do
        it "should find simple paths from graphs with two nodes" $ do 
            let g = M.fromList [(1,[(2,100)]),(2,[(1,100)])]
            shortestPaths g 1  `shouldBe` [(1,0,1),(2,100,1)] 

            let g = M.fromList [(5,[(7,130)]),(7,[(5,130)])]
            shortestPaths g 5  `shouldBe` [(5,0,5),(7,130,5)] 
