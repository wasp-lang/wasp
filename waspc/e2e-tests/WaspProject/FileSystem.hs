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

data SeedsDir

data SeedsFile

seedsDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SeedsDir)
seedsDirInWaspProjectDir = [reldir|src/db|]

seedsFileInSeedsDir :: Path' (Rel SeedsDir) (File SeedsFile)
seedsFileInSeedsDir = [relfile|seeds.ts|]