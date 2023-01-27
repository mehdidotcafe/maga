{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

solve :: Double -> Double -> Double -> Double
solve meal_cost tip_percent tax_percent = meal_cost + (meal_cost * tip_percent / 100) + (meal_cost * tax_percent / 100)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    meal_costTemp <- getLine
    let meal_cost = read $ lstrip $ rstrip meal_costTemp :: Double

    tip_percentTemp <- getLine
    let tip_percent = read $ lstrip $ rstrip tip_percentTemp :: Double

    tax_percentTemp <- getLine
    let tax_percent = read $ lstrip $ rstrip tax_percentTemp :: Double

    print $ round $ solve meal_cost tip_percent tax_percent
