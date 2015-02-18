module Specs
where
import Test.Hspec
import Maze
import Data.Map

main = hspec $ do
    describe "maze" $ do
        it "should read a very simple maze" $ do
            maze "AB" `shouldBe` 
                Maze { a = (0,0),
                       b = (0,1),
                       rooms = [(0,0),(0,1)] }

        it "should find A and B locations" $ do
            maze "BA" `shouldBe` 
                Maze { a = (0,1),
                       b = (0,0),
                       rooms = [(0,0),(0,1)] }

        it "should read lines longer than 2" $ do
            maze "A.B" `shouldBe` 
                Maze { a = (0,0),
                       b = (0,2),
                       rooms = [(0,0),(0,1),(0,2)] }

        it "should read more than 1 line" $ do
            maze "A\nB" `shouldBe` 
                Maze { a = (0,0),
                       b = (1,0),
                       rooms = [(0,0),(1,0)] }

        it "should ignore walls" $ do
            maze "A#B" `shouldBe` 
                Maze { a = (0,0),
                       b = (0,2),
                       rooms = [(0,0),(0,2)] }

        it "should process e.g. \n##A##\n#...#\n#.#.#\n###B#\ncorrectly" $ do
            let t="##A##\n#...#\n#.#.#\n###B#"
            maze t `shouldBe` 
                Maze {a = (0,2), 
                      b = (3,3), 
                      rooms = [(0,2),(1,1),(1,2),(1,3),(2,1),(2,3),(3,3)]}

    describe "nodes" $ do
        it "should collect all rooms from a maze" $ do
        let m = Maze (0,0) (1,1) [(0,0),(1,0),(1,1)]
        nodes m `shouldBe` fromList [((0,0),[]),((1,0),[]),((1,1),[])]

    describe "adjList" $ do
        it "should convert very simple mazes" $ do
            let m = Maze (0,0) (0,1) [(0,0),(0,1)]
                n = Maze (0,0) (1,0) [(0,0),(1,0)]
            adjList m `shouldBe` [((0,0),(0,1),1),((0,1),(0,0),1)] 
            adjList n `shouldBe` [((0,0),(1,0),1),((1,0),(0,0),1)] 

        it "should not connect rooms separated by walls" $ do
            let m = Maze (0,0) (0,2) [(0,0),(0,2)]
            adjList m `shouldBe` []

        it "should connect rooms in both directions" $ do
            let m = Maze (0,0) (1,1) [(0,0),(1,0),(1,1)]
            adjList m `shouldBe` 
                [((0,0),(1,0),1),((1,0),(0,0),1),((1,0),(1,1),1),((1,1),(1,0),1)]
