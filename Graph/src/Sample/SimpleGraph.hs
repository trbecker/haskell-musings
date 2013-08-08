module SampleGraph (
	module Data.Graph,
	module Data.FiniteMap,
	module Data.Graph.Simple
	) where

import Data.Graph
import Data.FiniteMap
import Data.Graph.Simple
import qualified Data.Map as M

type SampleNode a = (a, [a], a)
type SampleEdge a = (a, (a, a), a)

sampleNodes = [
	(1, [1],    1),
	(2, [1, 3], 2),
	(3, [4, 2], 17),
	(4, [2, 3], 3)]

sampleEdges = [
	(1, (1, 2), 3),
	(2, (3, 4), 12),
	(3, (4, 2), 4),
	(4, (2, 3), 5)]

edgeMap :: M.Map Integer (Integer, (Integer, Integer), Integer)
edgeMap = M.fromList $ map assocEdge sampleEdges
	where assocEdge (a, b, c) = (a, (a, b, c))

nodeMap :: M.Map Integer (Integer, [Integer], Integer)
nodeMap = M.fromList $ map assocNode sampleNodes
	where assocNode (a, b, c) = (a, (a, b, c))

graph :: (M.Map Integer (SampleNode Integer), M.Map Integer (SampleEdge Integer))
graph = (nodeMap, edgeMap)