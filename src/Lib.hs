module Lib
  ( printFileChecksums,
  )
where

import Control.Monad (filterM, liftM2)
import Crypto.Hash (Digest, SHA256 (..), hashWith)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Path as P
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.IO ()

type AbsPathToFile = SP.Path' SP.Abs (SP.File ())

type AbsPathToDir = SP.Path' SP.Abs (SP.Dir ())

type Checksum = String

printFileChecksums :: FilePath -> IO ()
printFileChecksums absDirFP =
  do
    absDirSP <- SP.parseAbsDir absDirFP
    filesWithChecksums <- walkDirs [] [absDirSP] (return [])
    mapM_ print filesWithChecksums

-- Gets all checksums for all descendant files (including in subdirectories)
walkDirs :: [AbsPathToFile] -> [AbsPathToDir] -> IO [(AbsPathToFile, Checksum)] -> IO [(AbsPathToFile, Checksum)]
walkDirs (fp : fps) dirs acc =
  do
    walkDirs fps dirs (liftM2 (:) (getFileChecksum fp) acc)
walkDirs [] (dp : dps) acc =
  do
    files <- listFilesInDirectory dp
    dirs <- listDirsInDirectory dp
    walkDirs files (dirs ++ dps) acc
walkDirs [] [] acc = acc

getFileChecksum :: AbsPathToFile -> IO (AbsPathToFile, Checksum)
getFileChecksum absFileSP =
  do
    contents <- B.readFile $ SP.fromAbsFile absFileSP
    let checksum = hashWith SHA256 contents
    return (absFileSP, show checksum)

-- TODO: Probably some nicer way to keep the shape of these two functions but
-- extract the boilerplate and parameterize functions used based on dir or file.
-- Also, not ideal to go from StrongPath, to FilePath, to StrongPath,
-- to FilePath, and back to StrongPath, but didn't see a clean interop method between them
-- to use listDirectory and doesXExist.
listFilesInDirectory :: AbsPathToDir -> IO [AbsPathToFile]
listFilesInDirectory absDirSP =
  do
    relFileFPs <- listDirectory $ SP.fromAbsDir absDirSP
    absFileSPs <- mapM (relToAbsFilePath absDirSP) relFileFPs
    absFileFPs <- filterM doesFileExist (map SP.fromAbsFile absFileSPs)
    mapM SP.parseAbsFile absFileFPs

listDirsInDirectory :: AbsPathToDir -> IO [AbsPathToDir]
listDirsInDirectory absDirSP =
  do
    relDirFPs <- listDirectory $ SP.fromAbsDir absDirSP
    absDirSPs <- mapM (relToAbsDirPath absDirSP) relDirFPs
    absDirFPs <- filterM doesDirectoryExist (map SP.fromAbsDir absDirSPs)
    mapM SP.parseAbsDir absDirFPs

-- Helpers to combine StrongPaths and FilePaths
relToAbsFilePath :: AbsPathToDir -> FilePath -> IO AbsPathToFile
relToAbsFilePath absDirSP relFileFP =
  do
    relFileSP <- SP.parseRelFile relFileFP
    return (absDirSP SP.</> relFileSP)

relToAbsDirPath :: AbsPathToDir -> FilePath -> IO AbsPathToDir
relToAbsDirPath absDirSP relDirFP =
  do
    relDirSP <- SP.parseRelDir relDirFP
    return (absDirSP SP.</> relDirSP)
