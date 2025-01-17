# Revision history for posixtime

## 0.1.5 --2020-02-19
	* listDirCompletePathR :: FilePath -> IO [FilePath]
	Return the absolute path of `FilePath` and recursive directories.
	* completePath :: FilePath -> IO [FilePath]
	Return the absolute path of `FilePath` in a directory.
	* getAccessTime :: FilePath -> IO (POSIXTime)
	Return the access time of the file.
	* getChangeTime :: FilePath -> IO (POSIXTime)
	Return the change time of the file.
	* getModificationTime :: FilePath -> IO (POSIXTime)
	Return the last modification time of the file.
	* posixToUTC :: POSIXTime -> UTCTime
	Return the transformation from `POSIXTime` in `UTCTime`.
	* getTimesL :: FilePath -> IO [(FilePath, String, String, String)]
	Return the `FilePath` and the access, modification and change time
	of the files in a directory.
	* getTimesR :: FilePath -> IO [(FilePath, String, String, String)]
	Return the `FilePath` and the access, modification and change time
	of the files in a directory, recursively.

## 0.1.4 --2020-02-18
	* getTimeL :: FilePath -> IO [(FilePath, UTCTime, UTCTime, UTCTime)]
	extract the "access time", "change time" and "modification time" of file in the local directory, the filename have the absolute path.
	* getTimeR :: FilePath -> IO [(FilePath, UTCTime, UTCTime, UTCTime)]
	extract the "access time", "change time" and "modification time" of file in the local and recursive directories,
	the filename have the absolute path.
	
## 0.1.3 --2020-02-17

	* listDirCompletePathR This function return the absolute path of
	the files recursively
	* completePath return the absolute path of the files of a directory
	* getAccessTimeR return the "accesstime" in UTC format of a files,
	recursively. Only return the filename of the file.
	* getAccessTimeFullPathR return the "accesstime" in POSIXTime of a
	directory recursively and return the absolute filename of the files.

## 0.1.2 --2020-02-13

* Adding a more readable print output

## 0.1.1 --2020-02-12

* Sub calculus for "files" time. The functions return in POSIXTime (Haskell data type)
  and the final output is in UTCTime (Haskell data type)

## 0.1.0  -- 2020-02-11

* First version. Released on an unsuspecting world.
