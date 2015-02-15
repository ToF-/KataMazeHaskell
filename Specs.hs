module Specs
where
import Test.Hspec
import Maze

main = hspec $ do
    describe "scanMaze" $ do
        it "should read a very simple maze" $ do
            scanMaze ["AB"] `shouldBe` 
                Maze { a = (0,0),
                       b = (0,1),
                       rooms = [(0,0),(0,1)] }

