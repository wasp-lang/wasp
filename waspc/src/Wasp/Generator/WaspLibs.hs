module Wasp.Generator.WaspLibs
  ( setUpLibs,
    waspLibsDeps,
    libsRootDirInGeneratedCodeDir,
  )
where

import Control.Exception (catch)
import Data.Maybe
import StrongPath
  ( Abs,
    Dir,
    Path',
    Rel,
    basename,
    fromRelFile,
    parseRelFile,
    reldir,
    (</>),
  )
import qualified Wasp.Data as Data
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Project.Common (generatedCodeDirInDotWaspDir)
import Wasp.Util.IO (copyDirectory)

data LibsSourceDir

data LibsRootDir

setUpLibs :: Path' Abs (Dir ProjectRootDir) -> IO [GeneratorError]
setUpLibs projectRootDir = do
  (setUpLibsIO >> return [])
    `catch` (\e -> return [GenericGeneratorError $ show (e :: IOError)])
  where
    setUpLibsIO :: IO ()
    setUpLibsIO = do
      libsSrcPath <- (</> libsDirPathInDataDir) <$> Data.getAbsDataDirPath
      let libsDstPath = projectRootDir </> libsRootDirInProjectRootDir
      copyDirectory libsSrcPath libsDstPath

-- TODO: dedupe this with the same logic in SDK
-- The libs need to follow the SDK location in the generated code directory.
libsRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir LibsRootDir)
libsRootDirInProjectRootDir =
  [reldir|../|]
    </> basename generatedCodeDirInDotWaspDir
    </> [reldir|libs|]

libsRootDirInGeneratedCodeDir :: Path' (Rel ProjectRootDir) (Dir LibsRootDir)
libsRootDirInGeneratedCodeDir = [reldir|libs|]

libsDirPathInDataDir :: Path' (Rel Data.DataDir) (Dir LibsSourceDir)
libsDirPathInDataDir = [reldir|Generator/libs|]

waspLibsDeps :: [Npm.Dependency.Dependency]
waspLibsDeps =
  Npm.Dependency.fromList
    -- TODO: we can probably generate the tarball name from the lib name on the left
    [ ("@wasp.sh/libs-auth", genWaspLibFileDep ("wasp.sh-libs-auth", "0.0.0"))
    ]

genWaspLibFileDep :: (String, String) -> String
genWaspLibFileDep (libName, libVersion) = "file:" ++ fromRelFile tarballPath
  where
    tarballPath = [reldir|../../libs|] </> fromJust (parseRelFile tarballFilename)
    tarballFilename = libName ++ "-" ++ libVersion ++ ".tgz"
