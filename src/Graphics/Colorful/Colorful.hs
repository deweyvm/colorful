{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , NoMonomorphismRestriction
           , FlexibleInstances
           , UndecidableInstances #-}

module Graphics.Colorful.Colorful where

import Control.Applicative((<$>))
import Control.Monad.Random.Class
import Data.Fixed (mod')
import Graphics.Colorful.Utils


singleUniform :: (MonadRandom m, Color c) => m c
singleUniform = do
    x <- getRandomR (0.0, 1.0)
    y <- getRandomR (0.0, 1.0)
    z <- getRandomR (0.0, 1.0)
    return $ mkColor x y z

uniform :: (MonadRandom m, Color c) => Int -> m [c]
uniform = generate singleUniform


singleOffset :: (MonadRandom m, Color c)
             => Double
             -> c
             -> m c
singleOffset offset seed = do
    let avg = average seed
    r <- getRandomR (0.0, 1.0)
    let newVal = avg + 2*r * offset - offset
    let ratio = newVal / (avg + 0.00001)
    return $ mapColor (*ratio) seed

offset :: (MonadRandom m, Color c) => Double -> c -> Int -> m [c]
offset ofs seed = generate (singleOffset ofs seed)

randomMix :: (Functor m, MonadRandom m, Color c)
          => [c]
          -> Double {- | "grey control" -}
          -> m c
randomMix cs g = do
    index <- getRandomR (0, length cs - 1)
    rands <- take (length cs) <$> getRandomRs (0.0, 1.0)
    let ratios = mapIth index id (*g) rands
    let sum' = sum ratios
    let divRatios = ratios <$$> (/sum')
    let mult f = sum $ zipWith (*) (f <$> cs) divRatios
    return $ mkColor (mult getX) (mult getY) (mult getZ)

triad :: (Functor m, MonadRandom m, Color c)
      => c
      -> c
      -> c
      -> Double
      -> m c
triad c0 c1 c2 = randomMix [c0, c1, c2]

singleHarmony :: (Functor m, MonadRandom m)
              => Double {- some random angle -}
              -> Double
              -> Double
              -> Double
              -> Double
              -> Double
              -> Double
              -> Double
              -> m ColorHSL
singleHarmony ref o1 o2 r0 r1 r2 sat lum = do
    r <- (* (r0 + r1 + r2)) <$> getRandomR (0.0, 1.0)
    let r' = if r > r0
             then if r < r0 + r1
                  then r + o1
                  else r + o2
             else r
    let hue = (ref + r') `mod'` 1.0
    return $ mkColorHSL hue sat lum

harmony :: (Functor m, MonadRandom m)
        => Double
        -> Double
        -> Double
        -> Double
        -> Double
        -> Double
        -> Double
        -> m ColorHSL
harmony o1 o2 r0 r1 r2 sat lum = do
    ref <- getRandomR (0.0, 1.0)
    singleHarmony ref o1 o2 r0 r1 r2 sat lum


singleRainbow :: MonadRandom m
              => Double
              -> Double
              -> Double
              -> Double
              -> Double
              -> Int
              -> Int
              -> m ColorHSL
singleRainbow hStart hEnd co sat lum i n = do
    let hr = hEnd - hStart
    let cellSize = hr / fromIntegral n
    let hue = cellSize * (fromIntegral i) + co + hStart
    return $ mkColorHSL hue sat lum
--fixme -- hue step should be baked relative to the nunmber of colors ??
rainbow :: MonadRandom m
        => Double {- | Start hue in [0,1) -}
        -> Double {- | End hue in [0,1), must be >= start hue -}
        -> Double {- | Cell offset = Random double in [0, hue step) -}
        -> Double {- | Sat -}
        -> Double {- | Lum -}
        -> Int    {- | Number of colors to generate-}
        -> m [ColorHSL]
rainbow hStart hEnd co sat lum n =
    generateC2 (singleRainbow hStart hEnd co sat lum) n

goldenRatioConjugate :: Double
goldenRatioConjugate = 0.618033988749895

singleGoldenRatio :: MonadRandom m
                  => Double
                  -> Double
                  -> Double
                  -> Int
                  -> m ColorHSL
singleGoldenRatio sat lum hue i = do
    let hue' = (hue + goldenRatioConjugate * (fromIntegral i)) `mod'` 1.0
    return $ mkColorHSL hue sat lum

{- | Generates a number of colors that are in high contrast to each other.
     Note, this is not very useful for more than 5-6 colors. -}
goldenRatioRainbow :: MonadRandom m
                   => Double {- | The saturation of the resulting colors. -}
                   -> Double {- | The lightness of the resulting colors. -}
                   -> Int    {- | The number of desired colors. -}
                   -> m [ColorHSL]
goldenRatioRainbow sat lum n = do
    hue <- getRandomR (0.0, 1.0)
    generateC (singleGoldenRatio sat lum hue) n

generate :: (MonadRandom m)
         => m c
         -> Int
         -> m [c]
generate gen =
    generateC2 (\_ _ -> gen)

generateC :: (MonadRandom m)
          => (Int -> m c)
          -> Int
          -> m [c]
generateC gen =
    generateC2 (\_ -> gen)


generateC2 :: (MonadRandom m)
           => (Int -> Int -> m c)
           -> Int
           -> m [c]
generateC2 gen n =
    helper [] 0 gen
    where helper acc i ma
              | i >= n = return acc
              | otherwise = do m <- ma i n
                               helper (m : acc) (i + 1) ma
