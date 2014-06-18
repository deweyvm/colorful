module Graphics.Colorful.Colorful where

import Control.Applicative((<$>), (<*>))
import Control.Monad.Random.Class

import Graphics.Colorful.Utils

uniform :: (Functor m, MonadRandom m) => Int -> m [ColorRGB]
uniform n = do
    rgbs <- triple <$> getRandomRs (0, 255)
    return $ take n $ mkColorRGB <$> rgbs

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

mapIth :: Int -> (a->b) -> (a->b) -> [a] -> [b]
mapIth index f g xs =
    let helper i (y:ys) =
          let h = if' f g (i == index) in
          h y : helper (i+1) ys
    in
    helper 0 xs

randomMix :: (Functor m, MonadRandom m)
          => [ColorRGB]
          -> Double {- "grey control" -}
          -> m ColorRGB
randomMix cs g = do
    index <- getRandomR (0, length cs - 1)
    rands <- getRandomRs (0.0, 1.0)
    let zs = zip rands cs
    let ratios = mapIth index
                        fst
                        ((*g) . fst)
                        zs
    let sum' = sum $ ratios
    let divRatios = (/sum') <$> ratios
    let mult f  = sum $ (\c -> sum $ (fromIntegral (f c) *) <$> divRatios) <$> cs
    return $ mkColorRGBd (mult getR, mult getG, mult getB)

colorTriad :: (Functor m, MonadRandom m)
           => ColorRGB
           -> ColorRGB
           -> ColorRGB
           -> Double
           -> m ColorRGB
colorTriad c1 c2 c3 = randomMix [c1, c2, c3]
