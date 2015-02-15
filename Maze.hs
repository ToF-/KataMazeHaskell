module Maze
where

data Maze = Maze { a :: Coords,
                   b :: Coords,
                   rooms :: [Coords] }
    deriving (Eq, Show)

type Coords = (Integer,Integer)

scanMaze :: [String] -> Maze
scanMaze _ = Maze (0,0) (0,1) [(0,0),(0,1)]
