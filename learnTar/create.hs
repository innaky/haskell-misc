module Create where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
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

newtype LinkTarget = LinkTarget BS.ByteString
 deriving (Eq, Ord, Show)

create :: FilePath -> FilePath -> [FilePath] -> IO ()
create tar base paths = BS.readFile tar . write =<< pack base paths

write :: [Entry] -> LBS.ByteString
write es = LBS.concat $ map putEntry es ++ [LBS.replicate (512*2) 0]



