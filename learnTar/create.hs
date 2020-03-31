module Create where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS


data Entry = Entry {
  entryTarPath :: {-# UNPACK #-} !TarPath,
  entryContent :: !EntryContent,
  entryPermissions :: {-# UNPACK #-} !Permissions,
  entryOwnership :: {-# UNPACK #-} !Ownership,
  entryTime :: {-# UNPACK #-} !EpochTime,
  entryFormat :: !Format
  }
  derivin (Eq, Show)

create :: FilePath -> FilePath -> [FilePath] -> IO ()
create tar base paths = BS.readFile tar . write =<< pack base paths

write :: [Entry] -> LBS.ByteString
write es = LBS.concat $ map putEntry es ++ [LBS.replicate (512*2) 0]



