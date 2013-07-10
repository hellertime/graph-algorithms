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

graph2 :: Graph Char
graph2 = foldl addEdge emptyGraph $ concat $ map toEdge edgeList
  where
    edgeList = [('a', "bc"), ('b', "d"), ('c', "de")]
    toEdge = \(s, ts) -> map (Edge (Node s) . Node) ts

data DFWState a = DFW { pre_i   :: Int
                      , rpost_j :: Int
                      , pre     :: [(Node a, Int)]
                      , rpost   :: [(Node a, Int)]
                      , visited :: Set (Node a)
                      , succs   :: [Node a]
                      , forest  :: Graph a
                      } deriving (Show)

mkDFWState j = DFW { pre_i = 1
                   , rpost_j = j
                   , pre = []
                   , rpost = []
                   , visited = Set.fromList []
                   , succs = []
                   , forest = emptyGraph
                   }

dfwalk :: Ord a => Graph a -> DFWState a
dfwalk g = foldl go s0 $ Set.toList $ nodes g
  where
    s0 = mkDFWState (Set.size $ nodes g)
    go s x = if Set.member x (visited s)
                 then s
                 else dfw (s { succs = successors x g }) g x

dfw :: Ord a => DFWState a -> Graph a -> Node a -> DFWState a
dfw s g n = s'' { rpost = (n,rpost_j s''):(rpost s''), rpost_j = rpost_j s'' - 1} 
  where
    s'' = foldl go s' $ succs s
    s' = s { visited = visited', pre_i = pre_i', pre = pre' }
    pre_i' = pre_i s + 1
    pre' = (n,pre_i s):(pre s)
    visited' = Set.insert n $ visited s
    go s y = if Set.member y (visited s)
                   then s
                   else dfw (s { forest = addEdge (forest s) (Edge n y)
                               , succs = successors y g }) g y
