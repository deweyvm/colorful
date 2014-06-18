module Graphics.Colorful.Utils where

import Control.Applicative((<$>))
import Control.Monad.Random.Class

data ColorRGB = ColorRGB Int Int Int deriving Show

getR :: ColorRGB -> Int
getR (ColorRGB r _ _) = r

getG :: ColorRGB -> Int
getG (ColorRGB _ g _) = g

getB :: ColorRGB -> Int
getB (ColorRGB _ _ b) = b

mapColorRGB :: (Int -> Int) -> (ColorRGB -> ColorRGB)
mapColorRGB f (ColorRGB r g b) = mkColorRGB (f r, f g, f b)

average :: ColorRGB -> Float
average (ColorRGB r g b) = (fromIntegral (r + g + b)) / 3.0

mkColorRGB :: (Int, Int, Int) -> ColorRGB
mkColorRGB (r, g, b) = ColorRGB (bound r) (bound g) (bound b)
    where bound x = min (max x 0) 255

mkColorRGBd :: (Double, Double, Double) -> ColorRGB
mkColorRGBd (r, g, b) = mkColorRGB (truncate r, truncate g, truncate b)


triple :: [a] -> [(a, a, a)]
triple (x:y:z:xs) = (x, y, z): triple xs
triple _ = []

if' :: a -> a -> Bool -> a
if' f t b = if b then t else f
