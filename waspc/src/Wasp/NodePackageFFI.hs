module Wasp.NodePackageFFI
  ( -- * Node Package FFI

    -- Provides utilities for running the Wasp-owned npm packages, and for
    -- inspecting the @\@wasp.sh/spec@ package installed in the user's project.
    RunnablePackage (..),
    getPackageBinName,
    getPackageProcessOptions,
    waspSpecPackageName,
    tryGettingInstalledWaspSpecVersion,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import StrongPath
  ( Abs,
    Dir,
    File,
    Path',
    castFile,
    fromAbsFile,
    parseRelDir,
    relfile,
    (</>),
  )
import qualified System.Process as P
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.IO as IOUtil

-- | These are the packages waspc runs as separate node processes. They are
-- published to npm and installed as dependencies of the @wasp@ npm package, so
-- they are installed next to the Wasp binary.
data RunnablePackage
  = DeployPackage
  | TsInspectPackage
  | -- | TODO(martin): I implemented this ts package because I planned to use prisma's TS sdk
    --   (@prisma/internals) inside it, but I ended up calling `prisma format` cli cmd directly,
    --   which means I could have really done it from Haskell!
    --   Therefore, reconsider if we should have this package, or if we should delete it and move
    --   this functionality here, into Haskell.
    --   It might make sense to keep it we will be maybe using @prisma/internals or some other
    --   prisma packages via it in the future, if not then it is not worth keeping it.
    PrismaPackage
  | WaspStudioPackage
  | WaspSpecPackage

-- | NOTE: These names must match the 'bin' fields in the packages' package.json files.
getPackageBinName :: RunnablePackage -> String
getPackageBinName DeployPackage = "__internal_wasp_deploy__"
getPackageBinName TsInspectPackage = "__internal_wasp_ts-inspect__"
getPackageBinName PrismaPackage = "__internal_wasp_prisma__"
getPackageBinName WaspStudioPackage = "__internal_wasp_studio-server__"
getPackageBinName WaspSpecPackage = "__internal_wasp_spec__"

-- | Get a 'P.CreateProcess' for a particular package.
--
-- We don't need to do any special setup because `npm` ensures that the
-- package's bin is available in the PATH.
getPackageProcessOptions :: RunnablePackage -> [String] -> P.CreateProcess
getPackageProcessOptions = P.proc . getPackageBinName

-- | NOTE: This must match the 'name' field in the spec package's package.json.
--
-- Besides being run by waspc, the spec package is also a regular dependency of
-- the user's project: that's how their @*.wasp.ts@ files import it.
waspSpecPackageName :: String
waspSpecPackageName = "@wasp.sh/spec"

-- | Reads the version of @\@wasp.sh/spec@ installed in the user's project.
tryGettingInstalledWaspSpecVersion ::
  Path' Abs (Dir WaspProjectDir) ->
  IO (Either String SV.Version)
tryGettingInstalledWaspSpecVersion projectDir = runExceptT $ do
  unlessM (liftIO $ IOUtil.doesFileExist packageJsonPath) $
    throwError $
      "Couldn't find " ++ fromAbsFile packageJsonPath
  packageJson <- ExceptT $ liftIO $ PJ.parsePackageJsonFile packageJsonPath
  ExceptT $ return $ case PJ.version packageJson of
    Just versionString -> first show $ SV.parseVersion versionString
    Nothing -> Left $ fromAbsFile packageJsonPath ++ " has no `version` field"
  where
    packageJsonPath :: Path' Abs (File InstalledPackageJsonFile)
    packageJsonPath =
      castFile $
        projectDir
          </> nodeModulesDirInWaspProjectDir
          </> fromJust (parseRelDir waspSpecPackageName)
          </> [relfile|package.json|]

data InstalledPackageJsonFile

instance PJ.PackageJsonFile InstalledPackageJsonFile
