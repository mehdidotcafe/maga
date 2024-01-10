import System.Environment ( getEnv )
import Data.List (sort)
import Data.Map (fromList, union, (!))

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= flip writeFile str

lilysHomework arr nb =
  let
    sortedMap = fromList $ zip [1..] (sort arr) 
    unsortedMap = fromList $ zip arr [1..]
    swappedUnsortedMap = fromList $ zip [1..] arr
    countSwap sm swum um i
      | i > nb = 0
      | otherwise = let
          j = sm!i
          l = um!j
          k = swum!i
          um'= union (fromList [(k, l), (j, i)])  um
          swum' = union (fromList [(l, k), (i, j)]) swum
        in if i == um!j then countSwap sm swum um (i + 1) else 1 + countSwap sm swum' um' (i + 1)
  in countSwap sortedMap swappedUnsortedMap unsortedMap 1

main = getContents >>=
    writeInEnvFile .
    show .
    (\(x:xs) ->  min (lilysHomework xs x) (lilysHomework (reverse xs) x)) .
    map (read :: String -> Int ) .
    words