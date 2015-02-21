module ShortestPath
where
import Data.Map (Map)
import qualified Data.Map as M (insert, keys)
import Data.PSQueue (PSQ)
import Data.PSQueue as Q (empty, insert, deleteMin, minView, Binding (..))


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
        where distance = if m == n then 0 else infinity

settle :: (Queue,Path) -> (Queue,Path)
settle (q,p) = case Q.minView q of
        Nothing             -> error "unexpected condition: empty PSQueue" 
        Just (m :-> d, q')  -> (q', M.insert m d p)
    
    
