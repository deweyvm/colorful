{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , NoMonomorphismRestriction
           , FlexibleInstances
           , UndecidableInstances #-}

module Graphics.Colorful.Colorful where

import Control.Applicative((<$>))
import Control.Monad.Random.Class

import Graphics.Colorful.Utils


uniform :: (MonadRandom m) => m ColorRGB
uniform = do
    r <- getRandomR (0, 255)
    g <- getRandomR (0, 255)
    b <- getRandomR (0, 255)
    return $ mkColorRGB r g b

withOffset :: (MonadRandom m, Color c)
           => Double
           -> c
           -> m c
withOffset offset seed = do
    let avg = average seed
    r <- getRandomR (0.0, 1.0)
    let newVal = avg + 2*r * offset - offset
    let ratio = newVal / (avg + 0.00001)
    return $ mapColor (*ratio) seed

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

generateColors :: (MonadRandom m)
               => m ColorRGB
               -> Int
               -> m [ColorRGB]
generateColors gen n =
    helper [] n gen
    where helper acc i ma
              | i <= 0 = return acc
              | otherwise = do m <- ma
                               helper (m : acc) (i - 1) ma

