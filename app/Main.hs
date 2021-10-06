module Main where

import Lib (printFileChecksums)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [absDirFP] -> printFileChecksums absDirFP
    _ -> error "Exactly one argument required: Absolute path to directory"
