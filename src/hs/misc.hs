import Data.Set (Set)

import Graph 
import Graph.Edge
import Graph.Node

import qualified Data.Set as Set

graph1 :: Graph Char
graph1 = foldl addEdge emptyGraph $ concat $ map toEdge edgeList
  where
    edgeList = [('a', "bc"), ('b', "d"), ('c', "de"), ('e', "a")]
    toEdge = \(s, ts) -> map (Edge (Node s) . Node) ts

data DFWState a = DFW { pre_i   :: Int
                      , rpost_j :: Int
                      , pre     :: [(Node a, Int)]
                      , rpost   :: [(Node a, Int)]
                      , visited :: Set (Node a)
                      , succs   :: [Node a]
                      , forest  :: Graph a
                      } deriving (Show)

dfw :: DFWState a -> Node a -> DFWState a
dfw s n =
  where
    visited' = Set.insert n $ visited s
    go x s@(_, _, _, _, v, d) y = if Set.member y v
                                      then s
                                      else
                                          
    (i + 1, j' - 1, (n,i):pre'', (n,j'):rpost'', visited'', d'')
