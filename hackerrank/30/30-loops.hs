{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List (map)
import Data.Text (unpack, stripStart, stripEnd, pack)

calcN n = map (\x -> nAsString ++ " x " ++ show x ++ " = " ++ show (n * x))
  where
    nAsString = show n

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
  nTemp <- getLine
  let n = read $ lstrip $ rstrip nTemp :: Int

  mapM_ putStrLn $ calcN n [1..10]
