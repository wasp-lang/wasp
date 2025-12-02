module WaspProject.FileSystem
  ( SeedsDir,
    SeedsFile,
    seedsDirInWaspProjectDir,
    mainWaspFileInWaspProjectDir,
    seedsFileInSeedsDir,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir, File', Path', Rel, parseRelFile, reldir, relfile)
import Wasp.Project.Common (WaspProjectDir)

data SeedsDir

data SeedsFile

seedsDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SeedsDir)
seedsDirInWaspProjectDir = [reldir|src/db|]

mainWaspFileInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
mainWaspFileInWaspProjectDir = [relfile|main.wasp|]

seedsFileInSeedsDir :: String -> Path' (Rel SeedsDir) File'
seedsFileInSeedsDir = fromJust . parseRelFile
