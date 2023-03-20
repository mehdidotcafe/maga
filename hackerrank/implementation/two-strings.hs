import Data.Set (fromList, intersection)
import Control.Monad ( forM_ )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

twoStrings s1 s2
  | null $ intersection ss1 ss2 = "NO"
  | otherwise = "YES"
  where
    ss1 = fromList s1
    ss2 = fromList s2

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    forM_ [1..q] $ \ q_itr -> getLine >>= (\ s1 -> getLine >>= hPutStrLn fptr . twoStrings s1 )

    hFlush fptr
    hClose fptr
