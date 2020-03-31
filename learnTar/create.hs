module Create where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

create :: FilePath -> FilePath -> [FilePath] -> IO ()
create tar base paths = BS.readFile tar . write =<< pack base paths

write :: [Entry] -> LBS.ByteString
write es = LBS.concat $ map putEntry es ++ [LBS.replicate (512*2) 0]


