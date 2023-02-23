{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( mapM )
import Data.List ( nub, sort, intercalate, unwords, isSuffixOf)
import Data.Text ( pack, stripEnd, stripStart, unpack)

allowedDomain = "@gmail.com"

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

readLines :: IO [(String, String)]
readLines = do

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    mapM (\n_itr -> do
        firstMultipleInputTemp <- getLine
        let firstMultipleInput = words $ rstrip firstMultipleInputTemp

        let firstName = firstMultipleInput !! 0

        let emailId = firstMultipleInput !! 1

        return (emailId, firstName)
      ) [1..n]

filterLines = sort . map snd . filter (isSuffixOf allowedDomain . fst)

main :: IO()
main = do
  lines <- readLines

  putStrLn $ intercalate "\n" $ filterLines lines