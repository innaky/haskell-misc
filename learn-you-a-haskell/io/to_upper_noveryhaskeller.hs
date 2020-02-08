import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
  inHandle <- openFile "input.txt" ReadMode
  outHandle <- openFile "output.txt" WriteMode
  mainloop inHandle outHandle
  hClose inHandle
  hClose outHandle

mainloop :: Handle -> Handle -> IO ()
mainloop inHandle outHandle =
  do ineof <- hIsEOF inHandle
     if ineof
       then return ()
       else do inpStr <- hGetLine inHandle
               hPutStrLn outHandle (map toUpper inpStr)
               mainloop inHandle outHandle
