import System.Environment ( getEnv )
import Data.List (isPrefixOf, findIndex)

filterIndexed p xs = [x | (x,i) <- zip xs [0..], p x i]

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= \filepath -> writeFile filepath str

balancedSums s = balancedSums' s 0 (sum s)
  where
    balancedSums' [] leftSum rightSum = "NO"
    balancedSums' (x:xs) leftSum rightSum
      | leftSum == (rightSum - x) = "YES"
      | otherwise = balancedSums' xs (leftSum + x) (rightSum - x)

main = getContents >>= writeInEnvFile .
  unlines .
  map (balancedSums . map (read :: String -> Int) . words) .
  filterIndexed (\ x y -> even y &&  (y /= 0)) .
  lines