module Graph
    (    -- ** Constructors 
         Graph (..)
    
         -- ** Utilities
    ,    addEdge
    ,    emptyGraph
    ,    successors
    )
  where

import Data.Set (Set)
import Graph.Edge (Edge)
import Graph.Node (Node)

import qualified Data.Set as Set
import qualified Graph.Edge as Edge
import qualified Graph.Node as Node

data Graph a = Graph { nodes :: Set (Node a)
                     , edges :: Set (Edge a)
                     } deriving (Show, Eq, Ord)

emptyGraph :: Ord a => Graph a
emptyGraph = Graph (Set.fromList []) (Set.fromList [])

addEdge :: Ord a => Graph a -> Edge a -> Graph a
addEdge g e = Graph nodes' edges'
  where
    nodes' = Set.union (Graph.nodes g) (Set.fromList [Edge.source e, Edge.target e])
    edges' = Set.insert e (Graph.edges g)

successors :: Eq a => Node a -> Graph a -> [Node a]
successors n = map Edge.target . Set.toList . Set.filter ((== n) . Edge.source) . edges
