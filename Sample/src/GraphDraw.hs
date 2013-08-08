{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module GraphDraw (
	module Data.Graph,
	module Data.FiniteMap
	) where

import Data.Graph
import qualified Data.Map as M
import Data.FiniteMap

import Graphics.Drawable
import Graphics.UI.GLUT
import Prelude hiding (lookup)

data NodeShape = Circle GLfloat | Square GLfloat GLfloat deriving (Show)
data EdgeShape = Arrow | NoArrow deriving (Show)
data Center = Center GLfloat GLfloat deriving (Show)

data DrawableNode a = DrawableNode Integer [Integer] NodeShape Center a deriving (Show)
data DrawableEdge a = DrawableEdge (Integer, Integer) Integer EdgeShape a deriving (Show)

instance Node (DrawableNode) Integer where
	nodeKey (DrawableNode k _ _ _ _) = k
	connectedEdges (DrawableNode _ es _ _ _) = es
	nodePayload (DrawableNode _ _ _ _ p) = p

instance Edge (DrawableEdge) Integer where
	edgeKey (DrawableEdge _ k _ _) = k

instance Num a => Weigthed (DrawableEdge a) a where
	weigth (DrawableEdge _ _ _ w) = w

instance Show a => Labeled (DrawableNode a) where
	label (DrawableNode _ _ _ _ l) = show l 

instance Graph (,) where
	nodes = fst
	edges = snd

sampleNodes :: [DrawableNode Integer]
sampleNodes = [
	DrawableNode 1 [1] (Circle 0.3)     (Center 0.0 0.1) 1,
	DrawableNode 2 [1] (Square 0.3 0.4) (Center 0.5 0.5) 2]

sampleEdges :: [DrawableEdge Integer]
sampleEdges = [DrawableEdge (1, 2) 1 Arrow 3]

graph = (M.fromList $ map assocNode sampleNodes, M.fromList $ map assocEdge sampleEdges)
	where
		assocNode n = (nodeKey n, n)
		assocEdge e = (edgeKey e, e)
