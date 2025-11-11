module WaspProject.FileSystem
  ( SeedsDir,
    SeedsFile,
    seedsDirInWaspProjectDir,
    seedsFileInSeedsDir,
  )
where

import Data.Maybe (fromJust)
import StrongPath
import Wasp.Project.Common (WaspProjectDir)

data SeedsDir

data SeedsFile

seedsDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SeedsDir)
seedsDirInWaspProjectDir = [reldir|src/db|]

seedsFileInSeedsDir :: String -> Path' (Rel SeedsDir) (File seedsFile)
seedsFileInSeedsDir = fromJust . parseRelFile
