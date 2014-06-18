module Graphics.Colorful.Utils where

import Control.Applicative((<$>))
import Control.Monad.Random.Class

data Color = Color Int Int Int deriving Show

getR :: Color -> Int
getR (Color r _ _) = r

getG :: Color -> Int
getG (Color _ g _) = g

getB :: Color -> Int
getB (Color _ _ b) = b

mapColor :: (Int -> Int) -> (Color -> Color)
mapColor f (Color r g b) = mkColor (f r, f g, f b)

average :: Color -> Float
average (Color r g b) = (fromIntegral (r + g + b)) / 3.0

mkColor :: (Int, Int, Int) -> Color
mkColor (r, g, b) = Color (bound r) (bound g) (bound b)
    where bound x = min (max x 0) 255

mkColorD :: (Double, Double, Double) -> Color
mkColorD (r, g, b) = mkColor (truncate r, truncate g, truncate b)


triple :: [a] -> [(a, a, a)]
triple (x:y:z:xs) = (x, y, z): triple xs
triple _ = []

if' :: a -> a -> Bool -> a
if' f t b = if b then t else f
