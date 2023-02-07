{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Prelude hiding (Left, Right)
import Data.Map
import Data.List ( foldr, map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

data Direction = Left | TopLeft | Top | TopRight | Right | BottomRight | Bottom | BottomLeft deriving (Eq, Ord, Show, Enum)

comp [yq, xq] n [yo, xo] l
  | xo < xq && yo == yq = insertWith min Left (xq - xo - 1) l -- Left
  | n - yo - xo == n - yq - xq && yo > yq && xq > xo = insertWith min TopLeft (min (yo - yq - 1) (xq - xo - 1)) l -- TopLeft
  | yo > yq && xo == xq = insertWith min Top (yo - yq - 1) l -- Top
  | yq - xq == yo - xo && yo > yq && xq < xo = insertWith min TopRight (min (yo - yq - 1) (xo - xq - 1)) l -- TopRight
  | xo > xq && yo == yq = insertWith min Right (xo - xq - 1)  l -- Right
  | n - yo - xo == n - yq - xq && yo < yq && xq < xo = insertWith min BottomRight (min (yq - yo - 1) (xo - xq - 1))  l -- BottomRight
  | yo < yq && xo == xq = insertWith min Bottom (yq - yo - 1)  l -- Bottom
  | yq - xq == yo - xo && yo < yq && xq > xo = insertWith min BottomLeft (min (yq - yo - 1) (xq - xo - 1))  l -- BottomLeft
  | otherwise = l

queensAttack q n l o =  Data.Map.foldr (+) 0 $ Data.List.foldr (comp q n) l o

initLines yq xq n = fromList [
  (Left, xq - 1),
  (TopLeft, min (xq - 1) (n - yq)),
  (Top, n - yq),
  (TopRight, min (n - yq) (n - xq)),
  (Right, n - xq),
  (BottomRight, min (yq - 1) (n - xq)),
  (Bottom, yq - 1),
  (BottomLeft, min (yq - 1) (xq - 1))
  ]


lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    secondMultipleInputTemp <- getLine
    let secondMultipleInput = words $ rstrip secondMultipleInputTemp

    let r_q = read (secondMultipleInput !! 0) :: Int

    let c_q = read (secondMultipleInput !! 1) :: Int

    obstaclesTemp <- readMultipleLinesAsStringArray k
    let obstacles = Data.List.map (Data.List.map (read :: String -> Int) . words . rstrip) obstaclesTemp

    let l = initLines r_q c_q n

    let result = queensAttack [r_q, c_q] n l obstacles

    print result

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
