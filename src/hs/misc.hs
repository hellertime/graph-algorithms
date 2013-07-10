import Graph 
import Graph.Edge
import Graph.Node

import qualified Data.Set as Set

graph1 :: Graph Char
graph1 = foldl addEdge emptyGraph $ concat $ map toEdge edgeList
  where
    edgeList = [('a', "bc"), ('b', "d"), ('c', "de"), ('e', "a")]
    toEdge = \(s, ts) -> map (Edge (Node s) . Node) ts
