module Wasp.NodePackageFFI
  ( -- * Node Package FFI

    -- Provides utilities for setting up and running node processes from the
    -- @packages/@ directory.
    RunnablePackage (..),
    getPackageProcessOptions,
  )
where

import qualified System.Process as P
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ

-- | These are the globally installed packages waspc runs directly from
-- their global installation path.
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

getRunnablePackageBinName :: RunnablePackage -> String
getRunnablePackageBinName DeployPackage = "__internal_wasp_deploy__"
getRunnablePackageBinName TsInspectPackage = "__internal_wasp_ts-inspect__"
getRunnablePackageBinName PrismaPackage = "__internal_wasp_prisma__"
getRunnablePackageBinName WaspStudioPackage = "__internal_wasp_studio-server__"
getRunnablePackageBinName WaspSpecPackage = "__internal_wasp_spec__"

-- | Get a 'P.CreateProcess' for a particular package.
--
-- We don't need to do any special setup because `npm` ensures that the
-- package's bin is available in the PATH.
getPackageProcessOptions :: RunnablePackage -> [String] -> P.CreateProcess
getPackageProcessOptions = P.proc . getRunnablePackageBinName

data InstalledPackageJsonFile

instance PJ.PackageJsonFile InstalledPackageJsonFile
