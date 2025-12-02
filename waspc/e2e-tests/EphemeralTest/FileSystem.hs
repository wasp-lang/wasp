module EphemeralTest.FileSystem
  ( EphemeralDir,
    ephemeralDirInE2eTestsDir,
    getEphemeralDir,
    EphemeralWaspProjectDir,
    asWaspProjectDir,
    ephemeralWaspProjectDirInEphemeralDir,
    ephemeralTestsTempDirInE2eTestsDir,
    getEphemeralTestsTempDir,
  )
where

import Data.Maybe (fromJust)
import FileSystem (E2eTestsDir, getE2eTestsDir)
import StrongPath (Abs, Dir, Path, Path', Rel, castDir, parseRelDir, reldir, (</>))
import Wasp.Project.Common (WaspProjectDir)

-- | The directory where all ephemeral tests' artifacts are stored.
data EphemeralTestsTempDir

ephemeralTestsTempDirInE2eTestsDir :: Path' (Rel E2eTestsDir) (Dir EphemeralTestsTempDir)
ephemeralTestsTempDirInE2eTestsDir = [reldir|EphemeralTest/temp|]

getEphemeralTestsTempDir :: IO (Path' Abs (Dir EphemeralTestsTempDir))
getEphemeralTestsTempDir = (</> ephemeralTestsTempDirInE2eTestsDir) <$> getE2eTestsDir

-- | The directory inside a 'EphemeralTestsTempDir' where artifacts for a specific ephemeral test are stored.
-- It is named after the ephemeral test name.
data EphemeralDir

ephemeralDirInE2eTestsDir :: String -> Path' (Rel E2eTestsDir) (Dir EphemeralDir)
ephemeralDirInE2eTestsDir testName = ephemeralTestsTempDirInE2eTestsDir </> (fromJust . parseRelDir $ testName)

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
