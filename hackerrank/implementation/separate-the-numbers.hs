{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.List (isPrefixOf)
import Data.Text ( pack, stripEnd, stripStart, unpack )

separateNumbers [] pfn = Nothing
separateNumbers (x:xs) pfn = if separateNumbers' xs (read fn :: Integer) then Just (pfn ++ [x]) else separateNumbers xs fn
  where
    fn = pfn ++ [x]
    separateNumbers' [] n = True
    separateNumbers' s' n
      | nextNAsString `isPrefixOf` s' = separateNumbers' (drop (length nextNAsString) s') nextN
      | otherwise = False
      where
        nAsString = show n    
        nextNAsString = show nextN
        nextN = n + 1 :: Integer

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    forM_ [1..q] $ \q_itr -> do
        s <- getLine

        case separateNumbers s "" of
          Nothing -> putStrLn "NO"
          Just v -> if v == s then putStrLn "NO" else putStrLn $ "YES " ++ v