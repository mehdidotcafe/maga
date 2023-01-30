{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Control.Monad ( forM_ )

utopianTree 0 = 1
utopianTree n 
  | even n = 1 + utopianTree (n - 1)
  | otherwise = 2 * utopianTree (n - 1)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    tTemp <- getLine
    let t = read $ lstrip $ rstrip tTemp :: Int

    forM_ [1..t] $ \t_itr -> do
        nTemp <- getLine
        let n = read $ lstrip $ rstrip nTemp :: Int

        let result = utopianTree n

        hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
