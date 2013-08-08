{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.FiniteMap where

import qualified Data.Map as M

class FiniteMap m a where
	(!) :: (Ord a) => m b -> a -> b
	insert :: (Ord a) => m b -> a -> b -> m b
	fromList :: [(a, b)] -> m b
	lookup :: (Ord a) => a -> m b -> Maybe b

instance (Ord a) => FiniteMap (M.Map a) a where
	(!) = (M.!)
	insert m k v = M.insert k v m
	fromList = M.fromList
	lookup = M.lookup