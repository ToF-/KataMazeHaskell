module ShortestPath
where
import Data.Map (Map)
import qualified Data.Map as M

type Node  = Integer
type Edge  = (Node, Node, Label)
type Label = Integer
type Graph = Map Node [(Node,Label)]
type Step  = (Node, Label, Node)

shortestPaths :: Graph -> Node -> [Step]
shortestPaths g s = [(s,0,s),(d,l,s)]
    where
    (d,l) = head $ M.findWithDefault [] s g
