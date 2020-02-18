module Main where

import System.Posix.Files
import System.FilePath.Posix (takeFileName)
import System.Environment
import Data.Time.Clock.POSIX
import Data.Time.Clock
import System.FilePath
import System.Directory (listDirectory, doesDirectoryExist)
import Control.Monad

listDirCompletePathR :: FilePath -> IO [FilePath]
listDirCompletePathR mainDir = do
  lastFileNames <- listDirectory mainDir
  paths <- forM lastFileNames $ \smallName -> do
    let path = mainDir </> smallName
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then listDirCompletePathR path
      else return [path]
  return (concat paths)

completePath :: FilePath -> IO [FilePath]
completePath mainDir = do
  simpleFileNames <- listDirectory mainDir
  allPaths <- forM simpleFileNames $ \simpleName -> do
    let path = mainDir </> simpleName
    return [path]
  return (concat allPaths)

getAccessTime :: FilePath -> IO (POSIXTime)
getAccessTime filePath =
  do stat <- getFileStatus filePath
     return (accessTimeHiRes stat)

getAccessTimeR :: FilePath -> IO [(FilePath, UTCTime)]
getAccessTimeR topPath = do
  allFiles <- listDirCompletePathR topPath
  alloutput <- forM allFiles $ \filename -> do
    accesstime <- getAccessTime filename
    return [((takeFileName filename), (posixToUTC accesstime))]
  return (concat alloutput)

getAccessTimeFullPathR :: FilePath -> IO [(FilePath, POSIXTime)]
getAccessTimeFullPathR topPath = do
  allFiles <- listDirCompletePathR topPath
  alloutput <- forM allFiles $ \filename -> do
    accesstime <- getAccessTime filename
    return [(filename, accesstime)]
  return (concat alloutput)

getChangeTime :: FilePath -> IO (POSIXTime)
getChangeTime filePath =
  do stat <- getFileStatus filePath
     return (statusChangeTimeHiRes stat)

getModificationTime :: FilePath -> IO (POSIXTime)
getModificationTime filePath =
  do stat <- getFileStatus filePath
     return (modificationTimeHiRes stat)

posixToUTC :: POSIXTime -> UTCTime
posixToUTC inputPosixTime = posixSecondsToUTCTime inputPosixTime

main :: IO ()
main = do
  (input:_) <- getArgs
  otheraccesstime <- getAccessTimeR input
  putStrLn (show input ++ ", " ++ show otheraccesstime)
