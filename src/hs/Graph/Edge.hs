module Graph.Edge
    (    Edge (..)

    ,    reverse
    )
  where

import Graph.Node (Node)
import Prelude hiding (reverse)

data Edge a = Edge { source :: Node a
                   , target :: Node a
                   } deriving (Show, Eq, Ord)

reverse :: Edge a -> Edge a
reverse (Edge s t) = Edge t s
