create :: FilePath -> FilePath -> [FilePath] -> IO ()
 .: Data.ByteString.writeFile :: FilePath -> ByteString -> IO () 
 .: Codec.Archive.Tar.Write.write :: [Entry] -> Data.ByteStrting.Lazy 
     .: Data.ByteString.Lazy.concat :: [ByteString] -> ByteString 
     .: Codec.Archive.Tar.Write.putEntry :: Entry -> Data.ByteString.Lazy 
         .: Codec.Archive.Tar.Write.putHeader ::  Entry -> Data.ByteString.ByteString 
             .: Data.ByteString.Lazy.Char8.pack :: String -> ByteString 
             .: Data.ByteString.Lazy.take :: Int64 -> ByteString -> ByteString
             .: Codec.Archiv.Tar.Write.putOct :: (Integral a, Show a) => FieldWidth -> a -> String
                .: Codec.Archive.Tar.Write.fill :: FieldWidth -> Char -> String
                .: Codec.Archive.Tar.putChar8 :: Char -> String
                .: Numeric.showOct :: (Integral a, Show a) => a -> ShowS
          .: Codec.Archive.Tar.Write.putHeaderNoChkSum :: Entry -> String
             .: Codec.Archive.Tar.Write.putBString :: FieldWidth -> Data.ByteString.ByteString -> String
                .: Data.ByteString.unpack :: ByteString -> [Word8]
                .: Data.ByteString.take :: Int64 -> ByteString -> ByteString
                .: Codec.Archive.Tar.Write.fill :: FieldWidth -> Chart -> String
                .: Data.ByteString.length :: ByteString -> Int
             .: Codec.Archive.Tar.Write.putOct (Integral a, Show a) => FieldWidth -> a -> String
                .: Codec.Archive.Tar.Write.fill :: FieldWidth -> Char -> String
                .: Codec.Archive.Tar.putChar8 :: Char -> String
             .: Codec.Archive.Tar.Write.fill :: FieldWidth -> Char -> ByteString
                .: Data.ByteString.Char8.replicate :: Int -> Char -> ByteString
             .: Codec.Archive.Tar.putChar8 :: Char -> String
          .: Data.List.foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
       .: fromIntegral :: (Integral a, Num b) => a -> b
       .: negate :: Num a => a -> a
   .: Data.ByteString.Char8.replicate :: Int -> Char -> ByteString
 .: Codec.Archive.Tar.Pack.pack :: FilePath -> [FilePath] -> IO [Entry]
     .: Codec.Archive.Tar.Pack.prepaprePaths :: FilePath -> [FilePath] -> IO [FilePath]
         .: System.Directory.doesDirectoryExist :: FilePath -> IO Bool
         .: Codec.Archive.Tar.Pack.interleave :: [IO a] -> [IO a]
         .: Codec.Archive.Tar.Pack.getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
         .: FilePath.Native.addTrailingPathSeparator :: FilePath -> FilePath
     .: Codec.Archive.Tar.Pack.packPaths :: FilePath -> [FilePath] -> IO [Entry]
         .: Codec.Archive.Tar.Pack.interleave :: [IO a] -> [IO a]
             .: System.IO.Unsafe.unsafeInterleaveIO :: IO a -> IO a
         .: Codec.Archive.Tar.Types.toTarPath :: Bool -> FilePath -> Either String TarPath
             .: Codec.Archive.Tar.Types.splitLongPath :: FilePath -> Either String TarPath
             .: System.FilePath.Posix.joinPath :: [FilePath] -> FilePath
             .: System.FilePath.Posix.splitDirectories :: FilePath -> [FilePath]
         .: Codec.Archive.Tar.Pack.packDirectoryEntry :: FilePath -> TarPath -> IO Entry
             .: Codec.Archive.Tar.Pack.getModTime :: FilePath -> IO EpochTime
             .: Codec.Archive.Tar.Types :: TarPath -> Entry
         .: Codec.Archive.Tar.Pack.packFileEntry :: FilePath -> TarPath -> IO Entry
             .: Codec.Archive.Tar.Pack.getModTime :: FilePath -> IO EpochTime
             .: System.Directory.getPermissions :: FilePath -> IO Permissions
             .: System.IO.openBinaryFile :: FilePath -> IOMode -> IO Handle
             .: System.IO.hFileSize ::Handle -> IO Integer
             .: Data.ByteString.hgetContents :: Handle -> IO ByteString
             .: Codec.Archive.Tar.Types :: TarPath -> EntryContent -> Entry
             .: Prelude.fromIntegral :: (Integral a, Num b) => a -> b
             .: Codec.Archive.Tar.Types.executableFilePermissions :: Permissions
             .: Codec.Archive.Tar.Types.ordinaryFilePermissions :: Permissions
         .: FilePath.Native.hasTrailingPathSeparator :: FilePath -> Bool

Types:
  FieldWidth :: Int
  ShowS :: String -> String
  TarPath Data.ByteString, Data.ByteString
  EpochTime :: Int64
  Permissions :: FileMode
