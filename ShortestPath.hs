module ShortestPath
where
import Data.Map (Map)
import qualified Data.Map as M (insert, keys, lookup)
import Data.PSQueue (PSQ)
import Data.PSQueue as Q (empty, insert, deleteMin, minView, Binding (..), lookup, adjust)


type Node  = Int
type Label = Int
type Distance = Int
type Edge  = (Node, Label)
type Graph = Map Node [Edge]
type Path  = Map Node (Distance, Node)
type Step  = (Node, (Distance, Node))
type Queue = PSQ Node (Distance, Node)

infinity :: Int
infinity = maxBound

initialize :: Graph -> Node ->  Queue
initialize g n = foldl insertNode Q.empty (M.keys g)
    where
    insertNode :: Queue -> Node -> Queue 
    insertNode q m = Q.insert m (distance,m) q
        where
        distance | m == n    = 0
                 | otherwise = infinity

settle :: (Queue,Path) -> (Queue,Path)
settle (q,p) = case Q.minView q of
    Just (m :-> d, q')  -> (q', M.insert m d p)
    Nothing             -> error "unexpected condition: empty PSQueue" 
    
neighbors :: Node -> Distance -> Graph -> [(Node,Distance)]
neighbors n d g = case M.lookup n g of 
    (Just es) -> map addDistance es
        where
        addDistance (m,e) = (m,e+d)    

    Nothing -> error "unexpected condition: node not found"

adjust :: Node -> [(Node,Distance)] -> Queue -> Queue
adjust n ds q = foldl (adjustNode n) q ds
    where
    adjustNode :: Node -> Queue -> (Node,Distance) -> Queue
    adjustNode n q (m,d) | (currentDistance m q) > d = Q.adjust (const (d,n)) m q
        where 
        currentDistance :: Node -> Queue -> Distance
        currentDistance m q = case Q.lookup m q of
            (Just (d',_)) -> d'
            Nothing -> error "unexpected condition: node not in queue"
    adjustNode n q _  = q

shortestPath :: Graph -> Queue -> Path -> (Queue, Path)
shortestPath g q p = settle (q,p)

