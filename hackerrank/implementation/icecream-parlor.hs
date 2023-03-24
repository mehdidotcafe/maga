import System.Environment ( getEnv )
import Data.List (transpose, elemIndex)

chunk _ [] = []
chunk n xs = first : chunk n rest where (first, rest) = splitAt n xs

splitSkip n xs = transpose $ chunk n xs

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= \filepath -> writeFile filepath str

icecreamParlor i [[m], _, [x]] = [-1]
icecreamParlor i [[m], n, x:xs] = case elemIndex (m - x) xs of
  Nothing -> icecreamParlor (i + 1) [[m], n, xs]
  Just v -> [i, i + v + 1]

main = getContents >>= writeInEnvFile .
  unlines .
  map (unwords . map show . icecreamParlor 1) .
  chunk 3 .
  map (map (read :: String -> Int) . words) .
  tail .
  lines