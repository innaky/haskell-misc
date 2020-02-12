module Main where

import System.Posix.Files
import System.Posix.Types
import System.Environment

getTimes :: FilePath -> IO (EpochTime, EpochTime, EpochTime)
getTimes filePath =
  do stat <- getFileStatus filePath
     return (accessTime stat, modificationTime stat, statusChangeTime stat)

main :: IO ()
main = do
  (input:_) <- getArgs
  outf <- getTimes input
  print outf
