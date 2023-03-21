{-# LANGUAGE TupleSections #-}

import Data.Map (fromListWith, unionWith)

makingAnagrams [s1, s2] = sum $ unionWith (\ x y -> abs (x - y)) ss1 ss2
  where
    ss1 = fromListWith (+) $ map (, 1) s1
    ss2 = fromListWith (+) $ map (, 1) s2

main = interact $ show . makingAnagrams . words