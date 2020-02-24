import System.IO
import System.Environment

main = do
  (filename:_) <- getArgs
  withFile filename ReadMode (\handle -> do
                                 contents <- hGetContents handle
                                 putStr contents)
{-
main = do
  (filename:_) <- getArgs
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
-}
