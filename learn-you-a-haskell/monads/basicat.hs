module Basicat where

import System.Environment
import System.Exit

content :: [[Char]] -> IO String
content ["-h"] = usage >> exit
content [""]   = usage >> exit
content []     = getContents
content fs     = concat `fmap` mapM readFile fs 

finish :: String -> String
finish x = x

usage = putStrLn "Usage: test [-h] file"
exit = exitWith ExitSuccess

main :: IO ()
main = getArgs >>= content >>= putStr . finish
