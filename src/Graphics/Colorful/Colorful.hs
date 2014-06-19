{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , NoMonomorphismRestriction
           , FlexibleInstances
           , UndecidableInstances #-}

module Graphics.Colorful.Colorful where

import Control.Applicative((<$>))
import Control.Monad.Random.Class

import Graphics.Colorful.Utils

uniform :: (Functor m, MonadRandom m) => Int -> m [ColorRGB]
uniform n = do
    rgbs <- triple <$> getRandomRs (0, 255)
    return $ take n $ uncurry3 mkColorRGB <$> rgbs

unif :: (Functor m, MonadRandom m) => m ColorRGB
unif = do
    r <- getRandomR (0, 255)
    g <- getRandomR (0, 255)
    b <- getRandomR (0, 255)
    return $ mkColorRGB r g b

withOffset :: (Functor m, MonadRandom m)
           => Float
           -> Int
           -> ColorRGB
           -> m [ColorRGB]
withOffset offset n c = do
    let avg = average c
    let makeOffset r =
            let newVal = avg + 2*r * offset - offset in
            let ratio = newVal / (avg + 0.00001) in
            mapColorRGB (\x -> truncate $ fromIntegral x *ratio) c
    rs <- getRandomRs (0.0, 1.0)
    return $ take n $ makeOffset <$> rs

mapIth :: Int -> (a -> b) -> (a -> b) -> [a] -> [b]
mapIth index f g xs =
    let helper i (y:ys) =
          let h = if' f g (i == index) in
          h y : helper (i+1) ys
        helper _ [] = []
    in
    helper 0 xs




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
    let mult f = sum $ (\(c, r) -> (f c) * r) <$> zip cs divRatios
    return $ mkColor (mult getX) (mult getY) (mult getZ)

generateColors :: (Functor m, MonadRandom m)
               => m ColorRGB
               -> Int
               -> m [ColorRGB]
generateColors gen n =
    helper [] n gen
    where helper acc i ma
              | i <= 0 = return acc
              | otherwise = do m <- ma
                               helper (m : acc) (i - 1) ma

