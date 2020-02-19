module Main where

import System.Posix.Files (getFileStatus, accessTimeHiRes, statusChangeTimeHiRes, modificationTimeHiRes)
import System.FilePath.Posix ((</>))
import System.Environment (getArgs)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, POSIXTime)
import Data.Time.Clock  (UTCTime)
import System.Directory (listDirectory, doesDirectoryExist)
import Control.Monad (forM)
import Data.Time.Format (formatTime, defaultTimeLocale)

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

getTimesL :: FilePath -> IO [(FilePath, String, String, String)]
getTimesL topPath = do
  localFiles <- completePath topPath
  allLocalFiles <- forM localFiles $ \filename -> do
    accesstime <- getAccessTime filename
    changetime <- getChangeTime filename
    modiftime <- getModificationTime filename
    return [(filename, (formatTime defaultTimeLocale "%a %b %d %T %Y" (posixToUTC accesstime)), (formatTime defaultTimeLocale "%a %b %d %T %Y" (posixToUTC changetime)), (formatTime defaultTimeLocale "%a %b %d %T %Y" (posixToUTC modiftime)))]
  return (concat allLocalFiles)

getTimesR :: FilePath -> IO [(FilePath, String, String, String)]
getTimesR topPath = do
  allFiles <- listDirCompletePathR topPath
  alloutput <- forM allFiles $ \filename -> do
    accesstime <- getAccessTime filename
    changetime <- getChangeTime filename
    modiftime <- getModificationTime filename
    return [(filename, (formatTime defaultTimeLocale "%a %b %d %T %Y" (posixToUTC accesstime)), (formatTime defaultTimeLocale "%a %b %d %T %Y" (posixToUTC changetime)), (formatTime defaultTimeLocale "%a %b %d %T %Y" (posixToUTC modiftime)))]
  return (concat alloutput)

main :: IO ()

main = do (actionName:_) <- getArgs
          case lookup actionName listOfActions of
            Just action -> action
            Nothing -> print "Unknow action"

listOfActions :: [(String, IO ())]

listOfActions = [
  ("local", do putStrLn "Input the path: "
               filePath <- getLine
               times <- getTimesL filePath
               mapM_ print times),
  ("recursive", do putStrLn "Input the main path: "
                   filePath <- getLine
                   times <- getTimesR filePath
                   mapM_ print times)]
