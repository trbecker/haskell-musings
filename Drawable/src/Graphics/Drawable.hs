module Graphics.Drawable (Drawable) where

-- An object that can be draw over a surface
class Drawable a where
	draw :: a -> IO ()