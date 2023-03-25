{-# LANGUAGE TupleSections #-}

import Data.Map.Strict (fromListWith, differenceWith, keys )
import System.Environment ( getEnv )

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= \filepath -> writeFile filepath str

missingNumbers (a, b) = keys $ differenceWith (\ a b -> let diff = abs (b - a) in if diff > 0 then Just diff else Nothing) (fl b) (fl a)
  where
    fl s = fromListWith (+) $ map (, 1) s
main = getContents >>= writeInEnvFile .
  unwords .
  map show .
  missingNumbers .
  (\ [_, a, _,  b] -> (arrToInt $ words a, arrToInt $ words b)) .
  lines
  where
    arrToInt = map (read :: String -> Int)