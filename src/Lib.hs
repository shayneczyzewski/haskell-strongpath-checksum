module Lib
  ( listDirContentsAndCalcChecksums,
  )
where

import Control.Monad (filterM)
import Crypto.Hash (SHA256 (..), hashWith)
import qualified Data.ByteString as B
import StrongPath (Abs, Dir', File', Path', Rel')
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import qualified System.FilePath.Posix as FP

type Checksum = String

listDirContentsAndCalcChecksums :: FilePath -> IO [(Path' Abs File', Checksum)]
listDirContentsAndCalcChecksums absDirFP = SP.parseAbsDir absDirFP >>= listDirContentsAndCalcChecksums'

listDirContentsAndCalcChecksums' :: Path' Abs Dir' -> IO [(Path' Abs File', Checksum)]
listDirContentsAndCalcChecksums' dir = do
  files <- listDirContents dir
  checksums <- mapM getFileChecksum files
  return $ zip files checksums

listDirContents :: Path' Abs Dir' -> IO [Path' Abs File']
listDirContents dir = do
  files <- listFilesInDirectory dir
  subdirs <- listDirsInDirectory dir
  filesFromSubdirs <- mapM listDirContents subdirs
  return $ files ++ concat filesFromSubdirs

getFileChecksum :: Path' Abs File' -> IO Checksum
getFileChecksum file = do
  contents <- B.readFile $ SP.fromAbsFile file
  return $ show $ hashWith SHA256 contents

listFilesInDirectory :: Path' Abs Dir' -> IO [Path' Abs File']
listFilesInDirectory absDirSP =
  do
    let absDirFP = SP.fromAbsDir absDirSP
    relFPs <- listDirectory absDirFP
    let absFPs = map (absDirFP FP.</>) relFPs
    absFileFPs <- filterM doesFileExist absFPs
    mapM SP.parseAbsFile absFileFPs

listDirsInDirectory :: Path' Abs Dir' -> IO [Path' Abs Dir']
listDirsInDirectory absDirSP =
  do
    let absDirFP = SP.fromAbsDir absDirSP
    relFPs <- listDirectory absDirFP
    let absFPs = map (absDirFP FP.</>) relFPs
    absDirFPs <- filterM doesDirectoryExist absFPs
    mapM SP.parseAbsDir absDirFPs
