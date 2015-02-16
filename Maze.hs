module Maze
where

data Maze = Maze { a :: Coords,
                   b :: Coords,
                   rooms :: [Coords] }
    deriving (Eq, Show)

type Coords = (Integer,Integer)

scanMaze :: String -> Maze
scanMaze = normalize . fst . foldl scanLine (defaultMaze,(0,0)) 
    where
    defaultMaze = Maze (-1,-1) (-1,-1) []
    normalize (Maze a b rooms) = Maze a b $ reverse rooms

scanLine :: (Maze,Coords) -> Char -> (Maze,Coords)
scanLine (m,(r,c))  '#' = (m, (r, c+1))
scanLine (m,(r,c)) '\n' = (m, (r+1, 0))
scanLine (m,(r,c))  ch  = (updateMaze (r, c) ch m, (r, c+1))
    where
    updateMaze cd '.' m = addRoom cd m 
    updateMaze cd 'A' m = addRoom cd (m { a = cd })
    updateMaze cd 'B' m = addRoom cd (m { b = cd })

    addRoom cd (Maze a b rs) = Maze a b $ cd:rs  


