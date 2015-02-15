module Maze
where

data Maze = Maze { a :: Coords,
                   b :: Coords,
                   rooms :: [Coords] }
    deriving (Eq, Show)

type Coords = (Integer,Integer)

defaultMaze :: Maze
defaultMaze = Maze (-1,-1) (-1,-1) []

scanMaze :: String -> Maze
scanMaze = normalize . fst . foldl scanLine (defaultMaze,(0,0)) 

normalize :: Maze -> Maze
normalize m = m { rooms = reverse (rooms m)}

scanLine :: (Maze,Coords) -> Char -> (Maze,Coords)
scanLine (m,(r,c))  '#' = (m, (r, c+1))
scanLine (m,(r,c)) '\n' = (m, (r+1, 0))
scanLine (m,(r,c))  ch  = (updateMaze m (r, c) ch, (r, c+1))

updateMaze :: Maze -> Coords -> Char -> Maze
updateMaze m cd '.' = addRoom cd m 
updateMaze m cd 'A' = addRoom cd (m { a = cd })
updateMaze m cd 'B' = addRoom cd (m { b = cd })

addRoom :: Coords -> Maze -> Maze
addRoom cd m@(Maze _ _ rs) = m { rooms = cd:rs } 


