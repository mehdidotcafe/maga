{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

dayFineValue = 15
monthFineValue = 500

monthFine d1 m1 y1 d2 m2 y2  = monthFineValue

noFine  d1 m1 y1 d2 m2 y2  = 0

lateWithinMonthFine d1 m1 y1 d2 m2 y2 = dayFineValue * (d1 - d2)

lateWithinYearFine d1 m1 y1 d2 m2 y2 = monthFineValue * (m1 - m2)

lateMaxFine d1 m1 y1 d2 m2 y2  = 10000

stateMachine = [
  -- y before
  [
    -- m before
    [
      -- day before,
      noFine,
      -- day equals
      noFine,
      -- day after
      noFine
    ],
    -- m equals,
    [
      -- day before,
      noFine,
      -- day equals
      noFine,
      -- day after
      noFine
    ],
    -- m after
    [
      -- day before,
      noFine,
      -- day equals
      noFine,
      -- day after
      noFine
    ]
  ],
  -- y equal
  [
    -- m before
    [
      -- day before,
      noFine,
      -- day equals
      noFine,
      -- day after
      noFine
    ],
    -- m equals,
    [
      -- day before,
      noFine,
      -- day equals
      noFine,
      -- day after
      lateWithinMonthFine
    ],
    -- m after
    [
      -- day before,
      lateWithinYearFine,
      -- day equals
      lateWithinYearFine,
      -- day after
      lateWithinYearFine
    ]
  ],
  -- y after
  [
    -- m before
    [
      -- day before,
      lateMaxFine,
      -- day equals
      lateMaxFine,
      -- day after
      lateMaxFine
    ],
    -- m equals,
    [
      -- day before,
      lateMaxFine,
      -- day equals
      lateMaxFine,
      -- day after
      lateMaxFine
    ],
    -- m after
    [
      -- day before,
      lateMaxFine,
      -- day equals
      lateMaxFine,
      -- day after
      lateMaxFine
    ]
  ]
  ]

cmp x y = case compare x y of
  LT -> 0
  EQ -> 1
  GT -> 2

libraryFine :: Int -> Int -> Int -> Int -> Int -> Int -> Int
libraryFine d1 m1 y1 d2 m2 y2 = f d1 m1 y1 d2 m2 y2
  where
    f = stateMachine!!cmp y1 y2!!cmp m1 m2!!cmp d1 d2

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let d1 = read (firstMultipleInput !! 0) :: Int

    let m1 = read (firstMultipleInput !! 1) :: Int

    let y1 = read (firstMultipleInput !! 2) :: Int

    secondMultipleInputTemp <- getLine
    let secondMultipleInput = words $ rstrip secondMultipleInputTemp

    let d2 = read (secondMultipleInput !! 0) :: Int

    let m2 = read (secondMultipleInput !! 1) :: Int

    let y2 = read (secondMultipleInput !! 2) :: Int

    let result = libraryFine d1 m1 y1 d2 m2 y2

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
