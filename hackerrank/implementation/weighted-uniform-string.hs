import Data.List (group, find, unlines)
import Control.Arrow ((&&&))
import Debug.Trace (trace)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

aAsciiValue = fromEnum 'a'

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= \filepath -> writeFile filepath str

cmp letters v = case find (\ (letterWeight, letterCount) -> v `rem` letterWeight == 0 && v <= letterWeight * letterCount) letters of
  Nothing -> "No"
  _ -> "Yes"

main = getContents >>= writeInEnvFile .
  unlines .
  ( \ (letters, values) -> map (cmp letters) values) .
  (( map (\ x -> (fromEnum (head x) - aAsciiValue + 1, length x)). group . head) &&& (map (read :: String -> Int) . (tail . tail))) .
  words