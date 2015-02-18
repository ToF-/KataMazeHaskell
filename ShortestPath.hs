module ShortestPath
where

data Node a = Node a
type Edge = (Node a, Node a, b)

fromList :: [(Node a, [Edge])]
