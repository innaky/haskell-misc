import Control.Monad
import Data.Char

main = forever $ do
  putStrLn "Give me some input: "
  l <- getLine
  putStrLn $ reverseWords l

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
