{-# LANGUAGE TupleSections #-}

import Data.Map ( fromListWith , filter, size)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Debug.Trace (trace)
import Prelude hiding (filter)

gameOfThrones s
  | even sLength && null (filter odd ss) = "YES"
  | odd sLength && size (filter odd ss) == 1 = "YES"
  | otherwise = "NO"
  where
    ss = fromListWith (+) $ map (, 1) s
    sLength = length s

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    getLine >>= hPutStrLn fptr  . gameOfThrones

    hFlush fptr
    hClose fptr
