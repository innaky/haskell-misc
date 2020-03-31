module Create where
import qualified Data.ByteString.Lazy as BS
create :: FilePath -> FilePath -> [FilePath] -> IO ()
create tar base paths = BS.readFile tar . write =<< pack base paths

