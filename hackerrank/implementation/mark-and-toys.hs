import System.Environment ( getEnv )
import Data.List (sort)

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= \filepath -> writeFile filepath str

maximumToys [_, k] prices = maximumToys' k sortedPrices
  where
    maximumToys' money [] = 0
    maximumToys' money (x:xs)
      | money - x <= 0 = 0
      | otherwise = 1 + maximumToys' (money - x) xs
    sortedPrices = sort prices

main = getContents >>= writeInEnvFile .
  show .
  (\ a -> maximumToys (head a) (last a)) .
  map (map (read :: String -> Int) . words) .
  lines 