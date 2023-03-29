import System.Environment ( getEnv )
import Data.List (sort, groupBy)

defaultWeight = 4

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= \filepath -> writeFile filepath str

maximumToys weight toys = length $ groupBy (\ a b -> b - a <= weight ) $ sort toys

main = getContents >>= writeInEnvFile .
  show .
  maximumToys defaultWeight .
  map (read :: String -> Int) .
  words .
  last .
  lines 