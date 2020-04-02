module Create where

import Data.Monoid (mempty)
import Numeric     (showOct)
import Data.List   (foldl')
import Data.Char   (ord)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Lazy  as LBS.Char8
import Data.Int (Int64)
import System.Posix.Type (FileMode)


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

