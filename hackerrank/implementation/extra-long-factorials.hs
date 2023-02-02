import GHC.Num (Integer)

extraLongFactorials n = product [1..n]

main = do
  ioN <- getLine

  let n = read ioN :: Integer

  print $ extraLongFactorials n