module Specs
where
import Test.Hspec
import Maze

main = hspec $ do
    describe "scanMaze" $ do
        it "should read a very simple maze" $ do
            scanMaze "AB" `shouldBe` 
                Maze { a = (0,0),
                       b = (0,1),
                       rooms = [(0,0),(0,1)] }

        it "should find A and B locations" $ do
            scanMaze "BA" `shouldBe` 
                Maze { a = (0,1),
                       b = (0,0),
                       rooms = [(0,0),(0,1)] }

        it "should read lines longer than 2" $ do
            scanMaze "A.B" `shouldBe` 
                Maze { a = (0,0),
                       b = (0,2),
                       rooms = [(0,0),(0,1),(0,2)] }

        it "should read more than 1 line" $ do
            scanMaze "A\nB" `shouldBe` 
                Maze { a = (0,0),
                       b = (1,0),
                       rooms = [(0,0),(1,0)] }

        it "should ignore walls" $ do
            scanMaze "A#B" `shouldBe` 
                Maze { a = (0,0),
                       b = (0,2),
                       rooms = [(0,0),(0,2)] }

        it "should process e.g. \n##A##\n#...#\n#.#.#\n###B#\ncorrectly" $ do
            let t="##A##\n#...#\n#.#.#\n###B#"
            scanMaze t `shouldBe` 
                Maze {a = (0,2), 
                      b = (3,3), 
                      rooms = [(0,2),(1,1),(1,2),(1,3),(2,1),(2,3),(3,3)]}
