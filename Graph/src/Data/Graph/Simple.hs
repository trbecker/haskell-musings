{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Graph.Simple where

import Data.Graph

instance Graph (,) where
	nodes = fst
	edges = snd

instance (Ord k) => Node ((,,) k [k]) k where
	nodeKey (k, _, _) = k
	connectedEdges (_, [e], _) = [e]
	nodePayload (_, _, p) = p

instance (Ord k) => Edge ((,,) k (k, k)) k where
	edgeKey (k, _, _) = k

instance Show s => Labeled (a, b, s) where
	label (_, _, l) = show l

instance Num n => Weigthed (a, b, n) n where
	weigth (_, _, n) = n

