module WaspProject.FileSystem
  ( 
    SeedsDir,
    SeedsFile,
    seedsDirInWaspProjectDir,
    seedsFileInSeedsDir,
  )
where
  
import StrongPath
import Wasp.Project.Common (WaspProjectDir)
import Data.Maybe (fromJust)

data SeedsDir

data SeedsFile

seedsDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SeedsDir)
seedsDirInWaspProjectDir = [reldir|src/db|]

seedsFileInSeedsDir :: String -> Path' (Rel SeedsDir) (File seedsFile)
seedsFileInSeedsDir = fromJust . parseRelFile