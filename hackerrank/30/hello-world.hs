main :: IO()
main = do
  line <- getLine

  putStrLn $ "Hello, World.\n" ++ line
