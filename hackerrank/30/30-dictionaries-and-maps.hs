import Data.Map (fromList, member, (!))
import Control.Exception (try)

readRows 0 input = return $ fromList input
readRows n input = do
  rawRow <- getLine
  let row = words rawRow

  readRows (n-1) ((row!!0, row!!1):input)

getName m name = case member name m  of
  True -> name ++ "=" ++ m ! name
  False -> "Not found" 

readNames m 0 output = return output
readNames m n output = do
  name <- try getLine :: IO (Either IOError String)

  case name of
    Left ex -> return output
    Right v -> readNames m (n-1) (getName m v : output)
  
main = do
  nbNameTemp <- getLine
  let nbName = read nbNameTemp :: Int

  rows <- readRows nbName []

  names <- readNames rows nbName []
  
  mapM_ putStrLn $ reverse names
