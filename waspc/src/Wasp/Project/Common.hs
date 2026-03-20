module Wasp.Project.Common
  ( WaspProjectDir,
    DotWaspDir,
    NodeModulesDir,
    CompileError,
    CompileWarning,
    UserPackageJsonFile,
    SrcTsConfigFile,
    WaspFilePath (..),
    WaspLangFile,
    WaspTsFile,
    findFileInWaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedAppDirInDotWaspDir,
    waspProjectDirFromGeneratedAppDir,
    dotWaspRootFileInWaspProjectDir,
    dotWaspInfoFileInGeneratedAppDir,
    userPackageJsonInWaspProjectDir,
    packageLockJsonInWaspProjectDir,
    nodeModulesDirInWaspProjectDir,
    srcDirInWaspProjectDir,
    prismaSchemaFileInWaspProjectDir,
    getSrcTsConfigInWaspProjectDir,
    srcTsConfigInWaspLangProject,
    srcTsConfigInWaspTsProject,
    waspProjectDirFromGeneratedAppComponentRootDir,
    generatedAppDirInWaspProjectDir,
    makeAppUniqueId,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import StrongPath (Abs, Dir, File, File', Path', Rel, fromAbsDir, reldir, relfile, toFilePath, (</>))
import System.Directory (doesFileExist)
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJsonFile)
import Wasp.ExternalConfig.TsConfig (TsConfigFile)
import qualified Wasp.Generator.Common as G.Common
import qualified Wasp.Util as U
import Wasp.Util.StrongPath (invertRelDir)

type CompileError = String

type CompileWarning = String

data WaspProjectDir -- Root dir of Wasp project, containing source files.

data NodeModulesDir

data DotWaspDir -- Here we put everything that wasp generates.

data UserPackageJsonFile

instance PackageJsonFile UserPackageJsonFile

data SrcTsConfigFile

instance TsConfigFile SrcTsConfigFile

data WaspFilePath
  = WaspLang !(Path' Abs (File WaspLangFile))
  | WaspTs !(Path' Abs (File WaspTsFile))

data WaspLangFile

data WaspTsFile

-- TODO: SHould this be renamed to include word "root"?
dotWaspDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir DotWaspDir)
dotWaspDirInWaspProjectDir = [reldir|.wasp|]

nodeModulesDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir NodeModulesDir)
nodeModulesDirInWaspProjectDir = [reldir|node_modules|]

generatedAppDirInDotWaspDir :: Path' (Rel DotWaspDir) (Dir G.Common.GeneratedAppDir)
generatedAppDirInDotWaspDir = [reldir|out|]

generatedAppDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir G.Common.GeneratedAppDir)
generatedAppDirInWaspProjectDir = dotWaspDirInWaspProjectDir </> generatedAppDirInDotWaspDir

-- TODO: This backwards relative path relies on multiple forward relative path
-- definitions. We should find a better way to express it (e.g., by somehow
-- calculating it from existing definitions)
waspProjectDirFromGeneratedAppComponentRootDir :: (G.Common.GeneratedAppComponentRootDir d) => Path' (Rel d) (Dir WaspProjectDir)
waspProjectDirFromGeneratedAppComponentRootDir = [reldir|../|] </> waspProjectDirFromGeneratedAppDir

waspProjectDirFromGeneratedAppDir :: Path' (Rel G.Common.GeneratedAppDir) (Dir WaspProjectDir)
waspProjectDirFromGeneratedAppDir = invertRelDir generatedAppDirInWaspProjectDir

dotWaspRootFileInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
dotWaspRootFileInWaspProjectDir = [relfile|.wasproot|]

dotWaspInfoFileInGeneratedAppDir :: Path' (Rel G.Common.GeneratedAppDir) File'
dotWaspInfoFileInGeneratedAppDir = [relfile|.waspinfo|]

userPackageJsonInWaspProjectDir :: Path' (Rel WaspProjectDir) (File UserPackageJsonFile)
userPackageJsonInWaspProjectDir = [relfile|package.json|]

getSrcTsConfigInWaspProjectDir :: WaspFilePath -> Path' (Rel WaspProjectDir) (File SrcTsConfigFile)
getSrcTsConfigInWaspProjectDir = \case
  WaspTs _ -> srcTsConfigInWaspTsProject
  WaspLang _ -> srcTsConfigInWaspLangProject

srcTsConfigInWaspLangProject :: Path' (Rel WaspProjectDir) (File SrcTsConfigFile)
srcTsConfigInWaspLangProject = [relfile|tsconfig.json|]

srcTsConfigInWaspTsProject :: Path' (Rel WaspProjectDir) (File SrcTsConfigFile)
srcTsConfigInWaspTsProject = [relfile|tsconfig.src.json|]

packageLockJsonInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
packageLockJsonInWaspProjectDir = [relfile|package-lock.json|]

prismaSchemaFileInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
prismaSchemaFileInWaspProjectDir = [relfile|schema.prisma|]

srcDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SourceExternalCodeDir)
srcDirInWaspProjectDir = [reldir|src|]

findFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File f) ->
  IO (Maybe (Path' Abs (File f)))
findFileInWaspProjectDir waspDir file = do
  let fileAbsFp = waspDir </> file
  fileExists <- doesFileExist $ toFilePath fileAbsFp
  return $ if fileExists then Just fileAbsFp else Nothing

-- Returns a unique id that can be used for global identification of a specific Wasp project.
-- Id is no longer than 30 chars, all of them ascii letters, numbers, hyphen or underscore.
-- It is designed this way to make it easily usable in many scenarios.
-- It contains app name (or big part of it), to make it also readable for humans.
-- It is not resistant to Wasp project moving or being renamed, and will change in that case.
makeAppUniqueId :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeAppUniqueId waspProjectDir appName = take 19 sanitizedAppName <> "-" <> take 10 projectPathHash
  where
    projectPathHash = U.hexToString $ U.checksumFromString $ fromAbsDir waspProjectDir
    sanitizedAppName = filter isSafeChar appName
    isSafeChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-'
