module Main where

import System.Posix.Files
import System.Posix.Types
import System.Environment
import Data.Time.Clock.POSIX
import Data.Time.Clock

getAccessTime :: FilePath -> IO (POSIXTime)
getAccessTime filePath =
  do stat <- getFileStatus filePath
     return (accessTimeHiRes stat)

getModificationTime :: FilePath -> IO (POSIXTime)
getModificationTime filePath =
  do stat <- getFileStatus filePath
     return (modificationTimeHiRes stat)

getChangeTime :: FilePath -> IO (POSIXTime)
getChangeTime filePath =
  do stat <- getFileStatus filePath
     return (statusChangeTimeHiRes stat)

-- Wrapper function
posixToUTC :: POSIXTime -> UTCTime
posixToUTC inputPosixTime = posixSecondsToUTCTime inputPosixTime

main :: IO ()
main = do
  (input:_) <- getArgs
  accessTime <- getAccessTime input
  modifTime <- getModificationTime input
  changeTime <- getChangeTime input
  putStrLn (show input ++ "\t Access: " ++ show (posixToUTC accessTime) ++ "\t Modify: " ++ show (posixToUTC modifTime) ++ "\t Change: " ++ show (posixToUTC changeTime))
