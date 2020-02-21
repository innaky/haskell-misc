main = do
  line <- getContents
  putStrLn $ shortLines line

shortLines :: String -> String
shortLines inputString =
  let localLine = lines inputString
      filteredLst = filter (\line -> length line < 10) localLine
  in unlines filteredLst
