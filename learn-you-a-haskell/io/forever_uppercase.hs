import Data.Char

main = do
  l <- getContents
  putStr $ map toUpper l
