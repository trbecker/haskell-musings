{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Graph where

import Prelude hiding (lookup)
import Data.FiniteMap
import Control.Exception

-- A graph is a pair of finite sets, the first of objects, the second of relations
-- between objects (edges). Nodes and edges need to be uniquely indentified, so the
-- choice of a finite map to a ordered value seems obvious.
class Graph g where
	nodes :: (Ord k, FiniteMap m k, Node n k) => g (m (n p)) (m e) -> m (n p)
	edges :: (Ord k, FiniteMap m k, Edge e k) => g (m n) (m (e p)) -> m (e p)
	nodeById :: (Ord k, FiniteMap m k, Node n k) => g (m (n p)) (m e) -> k -> Maybe (n p)
	nodeById g i = lookup i $ nodes g

	edgeById :: (Ord k, FiniteMap m k, Edge e k) => g (m n) (m (e p)) -> k -> Maybe (e p)
	edgeById g i = lookup i $ edges g

-- A node 
class Node n k | n -> k where
	nodeKey :: (Ord k) => n p -> k
	connectedEdges :: (Ord k) => n p -> [k]
	nodePayload :: n p -> p

class Edge e k | e -> k where
	edgeKey :: (Ord k) => e p -> k

class Labeled o where
	label :: o -> String

class Weigthed o n | o -> n where
	weigth :: (Num n) => o -> n

