import System.Environment ( getEnv )
import Data.Set (fromList, size)

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= \filepath -> writeFile filepath str

stringConstruction s = size $ fromList s

main = getContents >>= writeInEnvFile .
  unlines .
  map (show . stringConstruction) .
  tail .
  lines