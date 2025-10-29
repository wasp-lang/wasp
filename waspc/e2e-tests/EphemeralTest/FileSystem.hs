module EphemeralTest.FileSystem
  ( EphemeralDir,
    ephemeralDirInE2eTestsDir,
    getEphemeralDir,
    EphemeralWaspProjectDir,
    asWaspProjectDir,
    ephemeralWaspProjectDirInEphemeralDir,
  )
where

import Data.Maybe (fromJust)
import FileSystem (E2eTestsDir, getE2eTestsDir)
import StrongPath (Abs, Dir, Path, Path', Rel, castDir, parseRelDir, (</>))
import Wasp.Project (WaspProjectDir)

data EphemeralDir

ephemeralDirInE2eTestsDir :: String -> Path' (Rel E2eTestsDir) (Dir EphemeralDir)
ephemeralDirInE2eTestsDir testName = fromJust . parseRelDir $ ("EphemeralTest/temp-" ++ testName)

getEphemeralDir :: String -> IO (Path' Abs (Dir EphemeralDir))
getEphemeralDir testName = (</> ephemeralDirInE2eTestsDir testName) <$> getE2eTestsDir

-- | The Wasp app directory inside of a 'EphemeralDir'.
data EphemeralWaspProjectDir

-- | Converts a 'EphemeralWaspProjectDir' to a 'Wasp.Project.Common.WaspProjectDir'.
-- This is safe because every ephemeral Wasp project directory is also a Wasp project directory.
asWaspProjectDir :: Path s a (Dir EphemeralWaspProjectDir) -> Path s a (Dir WaspProjectDir)
asWaspProjectDir = castDir

ephemeralWaspProjectDirInEphemeralDir :: String -> Path' (Rel EphemeralDir) (Dir EphemeralWaspProjectDir)
ephemeralWaspProjectDirInEphemeralDir ephemeralWaspProjectName = fromJust . parseRelDir $ ephemeralWaspProjectName
