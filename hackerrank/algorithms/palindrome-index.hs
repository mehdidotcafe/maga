import Data.List (intercalate)
import System.Environment (getEnv)
import Data.Bifunctor (first)

writeInEnvFile str = getEnv "OUTPUT_PATH" >>= flip writeFile str

notFound = (-1, "left")

isPalindrome [] [] = True
isPalindrome x [] = True
isPalindrome [] y = True
isPalindrome (x:xs) (y:ys)
  | x == y = isPalindrome xs ys
  | otherwise = False

pi' :: String -> String -> (Int, String)
pi' [] [] = notFound
pi' x [] = notFound
pi' [] y = notFound
pi' (x:xs) (y:ys)
  | x /= y && isPalindrome (x:xs) ys =  (0, "right")
  | x /= y && isPalindrome xs (y:ys) =  (0, "left")
  | otherwise = case pi' xs ys of
      (-1, "left") -> notFound
      k -> first (+ 1) k

palindromeIndex :: String -> Int
palindromeIndex s = let
  (left, right) = splitAt (length s `div` 2) s
  in case pi' left (reverse right) of
    (i, "right") -> length s - 1 - i
    (i, "left") -> i

main =
  getContents
    >>= writeInEnvFile
      . intercalate "\n"
      . map (show . (\s -> if isPalindrome s (reverse s) then fst notFound else palindromeIndex s))
      . tail
      . words