{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )

bsort s = case bsort' s of
               (c, a) | a == s    -> (c, a)
                 | otherwise -> case bsort a of (c', a') -> (c + c', a')
  where bsort' (x:x2:xs) | x > x2    = case bsort' (x:xs) of (c, a) -> (1 + c, x2:a)
                         | otherwise = case bsort' (x2:xs) of (c, a) -> (c, x:a)
        bsort' s = (0, s)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    aTemp <- getLine

    let a = map (read :: String -> Int) . words $ rstrip aTemp

    let (count, sa) = bsort a

    putStrLn $ "Array is sorted in " ++ show count ++ " swaps."
    putStrLn $ "First Element: " ++ show (head sa)
    putStrLn $ "Last Element: " ++ show (last sa)