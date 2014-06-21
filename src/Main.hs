
import Control.Monad.Random
import System.Exit
import qualified Graphics.Colorful.Colorful as Color
import Graphics.Colorful.Utils

main :: IO ()
main = do
    g <- getStdGen
    let c0 = mkColorRGB 15 25 18
    let c1 = mkColorRGB 115 25 18
    let c2 = mkColorRGB 15 125 118

    let r = evalRand (Color.uniform 10) g :: [ColorHSL]
    let r2 = evalRand (Color.generate (Color.randomMix [c0, c1, c2] 0.01) 10) g
    putStrLn $ "Result is : " ++ show r
    putStrLn $ "Result is : " ++ show r2
    print $ (fromHSL . toHSL) c0
    exitSuccess
