
import Control.Monad.Random
import System.Exit
import qualified Graphics.Colorful.Colorful as Color

main :: IO ()
main = do
    g <- getStdGen
    let r = evalRand (Color.uniform 3) g
    putStrLn $ "Result is : " ++ show r
    exitSuccess
