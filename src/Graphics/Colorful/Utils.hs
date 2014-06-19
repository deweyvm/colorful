module Graphics.Colorful.Utils where

import Control.Applicative((<$>))

triple :: [a] -> [(a, a, a)]
triple (x:y:z:xs) = (x, y, z): triple xs
triple _ = []

bound :: Ord a => a -> a -> a -> a
bound min' max' = min max' . max min'

if' :: a -> a -> Bool -> a
if' f t b = if b then t else f

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) x f = f <$> x

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f = (\(x, y, z) -> f x y z)



data ColorRGB = ColorRGB Int Int Int deriving Show

getR :: ColorRGB -> Int
getR (ColorRGB r _ _) = r

getG :: ColorRGB -> Int
getG (ColorRGB _ g _) = g

getB :: ColorRGB -> Int
getB (ColorRGB _ _ b) = b

mapColorRGB :: (Int -> Int) -> (ColorRGB -> ColorRGB)
mapColorRGB f (ColorRGB r g b) = mkColorRGB (f r) (f g) (f b)

average :: ColorRGB -> Float
average (ColorRGB r g b) = (fromIntegral (r + g + b)) / 3.0

mkColorRGB :: Int -> Int -> Int -> ColorRGB
mkColorRGB r g b = ColorRGB (bound' r) (bound' g) (bound' b)
    where bound' = bound 0 255

mkColorRGBd :: Double -> Double -> Double -> ColorRGB
mkColorRGBd r g b = mkColorRGB (truncate r) (truncate g) (truncate b)

data ColorHSL = ColorHSL Double Double Double


getH :: ColorHSL -> Double
getH (ColorHSL h _ _) = h

getS :: ColorHSL -> Double
getS (ColorHSL _ s _) = s

getL :: ColorHSL -> Double
getL (ColorHSL _ _ l) = l

mkColorHSL :: Double -> Double -> Double -> ColorHSL
mkColorHSL h s l = ColorHSL (bound' h) (bound' s) (bound' l)
    where bound' = bound 0.0 1.0

toHSL :: ColorRGB -> ColorHSL
toHSL (ColorRGB r g b) =
    let toF =(/256.0) . fromIntegral
        r' = toF r
        g' = toF g
        b' = toF b
        minc = minimum [r', g', b']
        maxc = maximum [r', g', b']
        l = (minc + maxc)/2
    in
    if maxc == minc
    then mkColorHSL 0 0 l
    else let d = maxc - minc
             s = if l > 0.5 then d/(2 - maxc - minc) else d/(maxc + minc)
             h = case () of
                   ()| maxc == r' -> (if' 0 6 (g < b)) + (g' - b')/d
                     | maxc == g' ->                 2 + (b' - r')/d
                     | maxc == b' ->                 4 + (r' - g')/d
         in
         mkColorHSL (h/6.0) s l

fromHSL :: ColorHSL -> ColorRGB
fromHSL (ColorHSL h s l) =
    let q = if (l < 0.5) then l * (1 + s) else l + s - l * s
        p = 2 * l - q
        r = hueToRGB p q (h + 1/3)
        g = hueToRGB p q h
        b = hueToRGB p q (h - 1/3)
    in
    mkColorRGBd (r*256) (g*256) (b*256)

    where hueToRGB p q t =
              let t' = wrapT t in
              case () of
                ()| t < 1/6   -> p + (q - p) * 6 * t'
                  | t < 1/2   -> q
                  | t < 2/3   -> p + (q - p) * (2/3 - t') * 6
                  | otherwise -> p
          wrapT t =
              case () of
                ()| t < 0 -> t + 1
                  | t > 1 -> t - 1
                  | otherwise -> t



class Color m where
    getX :: m -> Double
    getY :: m -> Double
    getZ :: m -> Double
    mkColor :: Double -> Double -> Double -> m

instance Color ColorRGB where
    getX = fromIntegral . getR
    getY = fromIntegral . getG
    getZ = fromIntegral . getB
    mkColor = mkColorRGBd

instance Color ColorHSL where
    getX = getH
    getY = getS
    getZ = getL
    mkColor = mkColorHSL

