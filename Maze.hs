module Maze
where
import Data.Map (Map,fromList, member)

data Maze = Maze { a :: Coords,
                   b :: Coords,
                   rooms :: [Coords] }
    deriving (Eq, Show)

type Coords = (Integer,Integer)

type AdjList = [Edge]

type Edge = (Node,Node,Weight)

type Node   = Coords
type Weight = Integer

maze :: String -> Maze
maze = normalize . fst . foldl scan (defaultMaze,(0,0)) 
    where
    defaultMaze = Maze (-1,-1) (-1,-1) []
    normalize (Maze a b rooms) = Maze a b $ reverse rooms

    scan :: (Maze,Coords) -> Char -> (Maze,Coords)
    scan (m,(r,c))  '#' = (m, (r, c+1))
    scan (m,(r,c)) '\n' = (m, (r+1, 0))
    scan (m,(r,c))  ch  = (updateMaze (r, c) ch m, (r, c+1))
        where
        updateMaze cd '.' m = addRoom cd m 
        updateMaze cd 'A' m = addRoom cd (m { a = cd })
        updateMaze cd 'B' m = addRoom cd (m { b = cd })

        addRoom cd (Maze a b rs) = Maze a b $ cd:rs  

nodes :: Maze -> Map Coords [Edge]
nodes (Maze _ _ rs) = fromList (map (\cd->(cd,[])) rs)

adjList :: Maze -> AdjList

adjList (Maze _ _ [r0,r1,r2]) = [(r0,r1,1),(r1,r0,1),(r1,r2,1),(r2,r1,1)]
adjList (Maze _ _ [r0,r1]) | neighbors r0 r1 = [(r0,r1,1),(r1,r0,1)]
    where neighbors (r0,c0) (r1,c1) = r0-r1 == 1 
                                   || c0-c1 == 1 
                                   || r1-r0 == 1 
                                   || c1-c0 == 1 
adjList _ = []


