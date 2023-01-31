import Control.Monad ( forM_ )
import Data.List ( map, intercalate )
splitStr [] idx = [[], []]
splitStr (x:xs) idx
  | even idx = [x:tailEven, tailOdd]
  | otherwise = [tailEven, x:tailOdd]
  where
    [tailEven, tailOdd] = splitStr xs (idx + 1)

main :: IO()
main = do
  nbRowTemp <- getLine
  let t = read nbRowTemp :: Int

  forM_ [1..t] $ \t_itr -> do
        str <- getLine

        putStrLn $ unwords $ splitStr str 0