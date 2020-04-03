{-# LANGUAGE CPP #-}

module Create where

import Data.Monoid (mempty)
import Numeric     (showOct)
import Data.List   (foldl')
import Data.Char   (ord)
import System.FilePath ((</>))
import System.Directory ( doesDirectoryExist, getModificationTime, Permissions(..), getPermissions,
                          getDirectoryContents )
import System.Posix.Type (FileMode)
import Data.Int (Int64)
import System.IO ( IOMode(ReadMode), openBinaryFile, hFileSize )
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Lazy  as LBS.Char8
import qualified System.FilePath       as FilePath.Native
       ( addTrailingPathSeparator, hasTrailingPathSeparator, joinPath, splitDirectories)
import qualified System.FilePath.Posix as FilePath.Posix
       (splitPath, joinPath)

data Entry = Entry {
  entryTarPath :: {-# UNPACK #-} !TarPath,
  entryContent :: !EntryContent,
  entryPermissions :: {-# UNPACK #-} !Permissions,
  entryOwnership :: {-# UNPACK #-} !Ownership,
  entryTime :: {-# UNPACK #-} !EpochTime,
  entryFormat :: !Format
  }
  deriving (Eq, Show)

data TarPath = TarPath {-# UNPACK #-} !BS.ByteString
                       {-# UNPACK #-} !BS.ByteString
  deriving (Eq, Ord)

data EntryContent = NormalFile LBS.ByteString {-# UNPACK #-} !FileSize
                    | Directory
                    | SymbolicLink    !LinkTarget
                    | HardLink        !LinkTarget
                    | CharacterDevice {-# UNPACK #-} !DevMajor
                                      {-# UNPACK #-} !DevMinor
                    | BlockDevice     {-# UNPACK #-} !DevMajor
                                      {-# UNPACK #-} !DevMinor
                    | NamedPipe
                    | OtherEntryType  {-# UNPACK #-} !TypeCode LBS.ByteString
                                      {-# UNPACK #-} !FileSize
  deriving (Eq, Ord, Show)

data Ownership = Ownership {
  ownerName :: String,
  groupName :: String,
  ownerId :: {-# UNPACK #-} !Int,
  groupId :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

data Format =
  V7Format
  | UstarFormat
  | GnuFormat
  deriving (Eq, Ord, Show)

type FileSize = Int64
type DevMajor = Int
type DevMinor = Int
type TypeCode = Char
type Permissions = FileMode
type EpochTime = Int64
type FieldWidth = Int

newtype LinkTarget = LinkTarget BS.ByteString
 deriving (Eq, Ord, Show)

create :: FilePath -> FilePath -> [FilePath] -> IO ()
create tar base paths = BS.readFile tar . write =<< pack base paths

-- write

write :: [Entry] -> LBS.ByteString
write es = LBS.concat $ map putEntry es ++ [LBS.replicate (512*2) 0]

putEntry :: Entry -> LBS.ByteString
putEntry entry = case entryContent entry of
  NormalFile content size       -> LBS.concat [ header, content, padding size ]
  OtherEntryType _ content size -> LBS.concat [ header, content, padding size ]
  _                             -> header
  where
    header       = putHeader entry
    padding size = LBS.replicate paddingSize 0
      where paddingSize = fromIntegral (negate size `mod` 512)

putHeader :: Entry -> LBS.ByteString
putHeader entry =
  LBS.Char8.pack $ take 148 block ++ putOct 7 checksum ++ ' ' : drop 156 block
  where
    block    = putHeaderNoChkSum entry
    checksum = foldl' (\x y -> x + ord y) 0 block

putHeaderNoChkSum :: Entry -> String
putHeaderNoChkSum Entry {
  entryTarPath     = TarPath name prefix,
  entryContent     = content,
  entryPermissions = permissions,
  entryOwnership   = ownership,
  entryTime        = modTime,
  entryFormat      = format
  } =
  concat
  [ putBString 100 $ name
  , putOct       8 $ permissions
  , putOct       8 $ ownerId ownership
  , putOct       8 $ groupId ownership
  , putOct      12 $ contentSize
  , putOct       8 $ modTime
  , fill         8 $ ' '
  , putChar8       $ typeCode
  , putBString 100 $ linkTarget
  ] ++
  case format of
    V7Format -> fill 255 '\NUL'
    UstarFormat -> concat
      [ putBString   8 $ ustarMagic
      , putString   32 $ ownerName ownership
      , putString   32 $ groupName ownership
      , putOct       8 $ deviceMajor
      , putOct       8 $ deviceMinor
      , putBString 155 $ prefix
      , fill        12 $ '\NUL'
      ]
    GNUFormat -> concat
      [ putBString   8 $ gnuMagic
      , putString   32 $ ownerName ownership
      , putString   32 $ groupName ownership
      , putGnuDev    8 $ deviceMajor
      , putGnuDev    8 $ deviceMinor
      , putBString 155 $ prefix
      , fill        12 $ '\NUL'
      ]
    where
      (typeCode, contentSize, linkTarget,
       deviceMajor, deviceMinor) = case content of
        NormalFile  _ size             -> ('0', size, mempty, 0, 0)
        Directory                      -> ('5', 0, mempty, 0, 0)
        SymbolicLink (LinkTarget link) -> ('2', 0, link, 0, 0)
        HardLink (LinkTarget link)     -> ('1', 0, link, 0, 0)
        CharacterDevice major minor    -> ('3', mempty, major, minor)
        BlockDevice major minor        -> ('4', 0, mempty, major, minor)
        NamedPipe                      -> ('6', 0, mempty, 0, 0)
        OtherEntryType code _ size     -> (code, size, mempty, 0, 0)

      putGnuDev w n = case content of
        CharacterDevice _ _ -> putOct w n
        BlockDevice     _ _ -> putOct w n
        _                   -> replicate w '\NUL'

ustarMagic, gnuMagic :: BS.ByteString
ustarMagic = BS.Char8.pack "ustar\NUL00"
gnuMagic   = BS.Char8.pack "ustar \NUL"

putBString :: FieldWidth -> BS.ByteString -> String
putBString n s = BS.Char8.unpack (BS.take n s) ++ fill (n - BS.length s) '\NUL'

putString :: FieldWidth -> String -> String
putString n s = take n s ++ fill (n - length s) '\NUL'

putOct :: (Integral a, Show a) => FieldWidth -> a -> String
putOct n x =
  let octStr = take (n - 1) $ showOct x ""
  in fill (n - length octStr - 1) '0'
  ++ octStr ++ putChar8 '\NUL'

putChar8 :: Char -> String
putChar8 c = [c]

fill :: FieldWidth -> Char -> String
fill n c = replicate n c

-- pack

pack :: FilePath -> [FilePath] -> IO [Entry]
pack baseDir paths0 = preparePaths baseDir paths0 >>= packPaths baseDir

preparePaths :: FilePath -> [FilePath] -> IO [FilePath]
preparePaths baseDir paths =
  fmap concat $ interleave
  [ do isDir <- doesDirectoryExist (baseDir (</>) path)
       if isDir
         then do entries <- getDirectoryContentsRecursive (baseDir </> path)
                 let entries' = map (path </>) entries
                     dir = FilePath.Native.addTrailingPathSeparator path
                 if null path then return 'entries
                   else return (dir : entries')
         else return [path]
  | path <- paths ]


packPaths :: FilePath -> [FilePath] -> IO [Entry]
packPaths baseDir paths =
  interleave
  [ do tarpath <- either fail return (toTarPath isDir relpath)
       if isDir 
         then packDirectoryEntry filepath tarpath
         else packFileEntry filepath tarpath
  | relpath <- paths
  , let isDir = FilePath.Native.hasTrailingPathSeparator filepath
        filepath = baseDir </> relpath ]

interleave :: [IO a] -> IO [a]
interleave = unsafeInterLeaveIO . go
  where
    go [] = return []
    go (x:xs) = do
      x' <- x
      xs' <- interleave xs
      return (x':xs')

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir0 =
  fmap tail (recurseDirectories dir0 [""])

recurseDirectories :: FilePath -> [FilePath] -> IO [FilePath]
recurseDirectories _ [] = return []
recurseDirectories base (dir:dirs) = unsafeInterLeaveIO $ do
  (files, dirs') <- collect [] [] =<< getDirectoryContents (base </> dir)

  files' <- recurseDirectories base (dirs' ++ dirs)
  return (dir : files ++ files')

  where
    collect files dirs' [] = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) | ignore entry = collect files dirs' entries
    collect files dirs' (entry:entries) = do
      let dirEntry = dir </> entry
          dirEntry' = FilePath.Native.addTrailingPathSeparator dirEntry
      isDirectory <- doesDirectoryExist (base </> dirEntry)
      if isDirectory
        then collect files (dirEntry':dirs') entries
        else collect (dirEntry:files) dirs' entries

    ignore ['.']      = True
    ignore ['.', '.'] = True
    ignore _          = False

toTarPath :: Bool -> FilePath -> Either String TarPath
toTarPath isDir = splitLongPath . addTrailingSep . FilePath.Posix.joinPath
                  . FilePath.Native.splitDirectories
  where
    addTrailingSep | isDir = FilePath.Posix.addTrailingPathSeparator
                   | otherwise = id

splitLongPath :: FilePath -> Either String TarPath
splitLongPath path =
  case packName nameMax (reverse (FilePath.Posix.splitPath path)) of
    Left err                 -> Left err
    Right (name, [])         -> Right $! TarPath (BS.Char8.pack name) BS.empty
    Right (name, first:rest) -> case packName prefixMax remainder of
      Left err               -> Left err
      Right (_, (_:_))       -> Left "File name too long (cannot split)"
      Right (prefix, [])     -> Right $! TarPath (BS.Char8.pack name) (BS.Char8.pack prefix)
      where
        remainder = init first : rest
  where
    nameMax, prefixMax :: Int
    nameMax   = 100
    prefixMax = 155
    
    packName _ [] = Left "File name empty"
    packName maxLen (c:cs)
      | n > maxLen = Left "File name too long"
      | otherwise  = Right (packName' maxLen n [c] cs)
      where n = length c
      
    packName' maxLen n ok (c:cs)
      | n' <= maxLen    = packName' maxLen n' (c:ok) cs
                            where n' = n + length c
    packName' _ _ ok cs = (FilePath.Posix.joinPath ok, cs)


packDirectoryEntry :: FilePath -> TarPath -> IO Entry
packDirectoryEntry filepath tarpath = do
  mtime <- getModTime filepath
  return (directoryEntry tarpath) {
    entryTime = mtime
  }

getModTime :: FilePath -> IO EpochTime
getModTime path = do
#if MIN_VERSION_directory(1,2,0)
  t <- getModificationTime path
  return . floor . utcTimeToPOSIXSeconds $ t
#else
  (TOD s _) <- getModificationTime path
  return $! fromIntegral s
#endif

packFileEntry :: FilePath -> TarPath -> IO Entry
packFileEntry filepath tarpath = do
  mtime <- getModTime filepath
  perms <- getPermissions filepath
  file <- openBinaryFile filepath ReadMode
  size <- hFileSize file
  content <- LBS.hGetContents file
  return (simpleEntry tarpath (NormalFile content (fromIntegral size))) {
    entryPermissions = if executable perms then executableFilePermissions
                                           else ordinaryFilePermissions,
    entryTime = mtime
  }

simpleEntry :: TarPath -> EntryContent -> Entry
simpleEntry tarpath content = Entry {
  entryTarPath     = tarpath,
  entryContent     = content,
  entryPermissions = case content of
                       Directory -> directoryPermissions
                       _         -> ordinaryFilePermissions,
  entryOwnership   = Ownership "" "" 0 0,
  entryTime        = 0,
  entryFormat      = UstarFormat
  }

directoryPermissions :: Permissions
directoryPermissions = 0o0755

ordinaryFilePermissions :: Permissions
ordinaryFilePermissions = 0o0644

executableFilePermissions :: Permissions
executableFilePermissions = 0o0755
