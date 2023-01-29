{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

catAndMouse x y z 
  | xzDiff < yzDiff = "Cat A"
  | yzDiff < xzDiff = "Cat B"
  | otherwise = "Mouse C"
  where 
    xzDiff = abs (x - z)
    yzDiff = abs (y - z)


main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    q <- readLn :: IO Int

    forM_ [1..q] $ \q_itr -> do
        xyzTemp <- getLine
        let xyz = words xyzTemp

        let x = read (xyz !! 0) :: Int

        let y = read (xyz !! 1) :: Int

        let z = read (xyz !! 2) :: Int

        print $ abs (x - z)
        print $ abs (y - z)

        let result = catAndMouse x y z

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
