import Data.List (findIndex)
appendAndDelete s t n
  | n >= length s + length t = "Yes"
  | d <= n && (d `mod` 2) == (n `mod` 2) = "Yes"
  | otherwise = "No"
  where
    d = length t - i + length s - i
    i = case findIndex (uncurry (/=)) st of
      Nothing -> min (length s) (length t) 
      Just v -> v
    st = zip s t

main = do
  s <- getLine
  t <- getLine
  ioN <- getLine

  let n = read ioN :: Int

  putStrLn $ appendAndDelete s t  n

