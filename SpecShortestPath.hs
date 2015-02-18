module SpecShortestPath
where
import Test.Hspec
import ShortestPath

main = hspec $ do
    describe "shortest path function" $ do
        it "should find source itself for trivial graph of one node (no path at all) " $ do
            let g = fromList [('A',[])]
            shortestPath g 'A' `shouldBe` (O, []) 
