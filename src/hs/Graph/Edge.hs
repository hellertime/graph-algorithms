module Graph.Edge
    (    Edge (..)
    )
  where

import Graph.Node (Node)

data Edge a = Edge { source :: Node a
                   , target :: Node a
                   } deriving (Show, Eq, Ord)
