import System.Environment ( getEnv )
import Data.List (sort, foldr)

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= \filepath -> writeFile filepath str

luckBalance [n, k] contests = sum notImportantContest + sum importantContestToLoose - sum importantContestToWin
  where
    importantContestToWin = take (length importantContest - k) sortedImportantContest
    importantContestToLoose = drop (length importantContest - k) sortedImportantContest
    sortedImportantContest = sort importantContest
    (importantContest, notImportantContest) = foldr (\ [contest, contestIsImportant] (ic, nic) -> if contestIsImportant == 1 then (contest : ic, nic) else (ic, contest : nic)) ([], []) contests

main = getContents >>= writeInEnvFile .
  show .
  (\a -> luckBalance (head a) (tail a)) .
  map (map (read :: String -> Int) . words) .
  lines 