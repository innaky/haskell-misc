module Main where

import System.Posix.Files
import System.Posix.Types
import System.Environment

getAccessTime :: FilePath -> IO (EpochTime)
getAccessTime filePath =
  do stat <- getFileStatus filePath
     return (accessTime stat)

getModificationTime :: FilePath -> IO (EpochTime)
getModificationTime filePath =
  do stat <- getFileStatus filePath
     return (modificationTime stat)

getChangeTime :: FilePath -> IO (EpochTime)
getChangeTime filePath =
  do stat <- getFileStatus filePath
     return (statusChangeTime stat)

main :: IO ()
main = do
  (input:_) <- getArgs
  accessTime <- getAccessTime input
  modifTime <- getModificationTime input
  changeTime <- getChangeTime input
  print [accessTime, modifTime, changeTime]
