module Main where

import Lib (listDirContentsAndCalcChecksums)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [absDirFP] -> listDirContentsAndCalcChecksums absDirFP >>= mapM_ print
    _ -> error "Exactly one argument required: Absolute path to directory"
