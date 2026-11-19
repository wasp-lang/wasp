{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.DockerGenerator
  ( genDockerFiles,
    genDockerfile,
    compileAndRenderDockerfile,
    DevWaspNpmPackagesDir,
    devWaspNpmPackagesDirInGeneratedAppDir,
  )
where

import Data.Aeson (object, (.=))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import StrongPath (Dir, File, File', Path', Rel, reldir, relfile)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Util (hasEntities)
import Wasp.AppSpec.Valid (getLowestNodeVersionUserAllows)
import Wasp.Generator.Common
  ( GeneratedAppDir,
    ServerRootDir,
  )
import Wasp.Generator.DbGenerator.Common
  ( PrismaDbSchema,
    dbSchemaFileFromGeneratedAppComponentDir,
  )
import Wasp.Generator.FileDraft (FileDraft (..), createTemplateFileDraft)
import qualified Wasp.Generator.FileDraft.TemplateFileDraft as TmplFD
import Wasp.Generator.Monad (Generator, GeneratorError, runGenerator)
import Wasp.Generator.Templates (TemplatesDir, compileAndRenderTemplate)
import Wasp.Project.PackageManager
  ( PackageManager (..),
    getLockfileName,
    getPackageManagerName,
    getSettingsFileNames,
    makeInstallCommand,
    makeLockedInstallCommand,
  )
import Wasp.Project.WaspNpmPackage (dependsOnDevWaspNpmPackages)

genDockerFiles :: AppSpec -> Generator [FileDraft]
genDockerFiles spec = sequence [genDockerfile spec, genDockerignore spec]

-- TODO: Inject paths to server and db files/dirs, right now they are hardcoded in the templates.
genDockerfile :: AppSpec -> Generator FileDraft
genDockerfile spec = do
  let dbSchemaFileFromServerDir :: Path' (Rel ServerRootDir) (File PrismaDbSchema) = dbSchemaFileFromGeneratedAppComponentDir
  return $
    createTemplateFileDraft
      ([relfile|Dockerfile|] :: Path' (Rel GeneratedAppDir) File')
      ([relfile|Dockerfile|] :: Path' (Rel TemplatesDir) File')
      ( Just $
          object
            [ "usingPrisma" .= hasEntities spec,
              "dbSchemaFileFromServerDir" .= SP.fromRelFile dbSchemaFileFromServerDir,
              "nodeVersion" .= show (getLowestNodeVersionUserAllows spec),
              "installPackageManagerCommand" .= makeInstallPackageManagerCommand packageManager (AS.pinnedPackageManagerVersion spec),
              "optionalInstallInputFiles" .= unwords (makeOptionalCopySource <$> installInputFileNames),
              "installCommand" .= makeInstallCommandForDocker [],
              -- Install scripts can read project files, so the first install
              -- in the Dockerfile, which runs before the code is in place,
              -- skips them.
              "installWithoutScriptsCommand" .= makeInstallCommandForDocker ["--ignore-scripts"],
              "dependsOnDevWaspNpmPackages" .= dependsOnDevWaspNpmPackages (AS.packageJson spec),
              "devWaspNpmPackagesDir" .= SP.fromRelDir devWaspNpmPackagesDirInGeneratedAppDir,
              "userDockerfile" .= fromMaybe "" (AS.userDockerfileContents spec)
            ]
      )
  where
    packageManager = AS.packageManager spec

    installInputFileNames = getLockfileName packageManager : getSettingsFileNames packageManager

    -- Follows the lockfile when the project has one. A project in a workspace
    -- has none of its own, its workspace root does.
    makeInstallCommandForDocker :: [String] -> String
    makeInstallCommandForDocker flags =
      "if [ -f "
        ++ getLockfileName packageManager
        ++ " ]; then "
        ++ unwords (makeLockedInstallCommand packageManager : flags)
        ++ "; else "
        ++ unwords (makeInstallCommand packageManager : flags)
        ++ "; fi"

-- | The Node.js image only comes with npm.
makeInstallPackageManagerCommand :: PackageManager -> Maybe String -> Maybe String
makeInstallPackageManagerCommand packageManager pinnedVersion = case packageManager of
  Npm -> Nothing
  _ -> Just $ unwords [makeInstallCommand Npm, "--global", packageManagerName ++ maybe "" ("@" ++) pinnedVersion]
  where
    packageManagerName = getPackageManagerName packageManager

-- | Docker fails to copy a file that doesn't exist, unless its name is a
-- pattern. A pattern that matches only this name is the name with a
-- character in square brackets.
makeOptionalCopySource :: FilePath -> String
makeOptionalCopySource fileName = init fileName ++ "[" ++ [last fileName] ++ "]"

data DevWaspNpmPackagesDir

-- | Where `wasp build` copies the dev tarballs of the unreleased Wasp npm
-- packages (see @Wasp.Cli.DevWaspNpmPackages@), for the Dockerfile to copy
-- them into the image.
devWaspNpmPackagesDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir DevWaspNpmPackagesDir)
devWaspNpmPackagesDirInGeneratedAppDir = [reldir|dev-packages|]

genDockerignore :: AppSpec -> Generator FileDraft
genDockerignore _ =
  return $
    createTemplateFileDraft
      ([relfile|.dockerignore|] :: Path' (Rel GeneratedAppDir) File')
      ([relfile|dockerignore|] :: Path' (Rel TemplatesDir) File')
      Nothing

-- | Helper to return what the Dockerfile content will be based on the AppSpec.
compileAndRenderDockerfile :: AppSpec -> IO (Either (NonEmpty GeneratorError) Text)
compileAndRenderDockerfile spec = do
  let (_, generatorResult) = runGenerator $ genDockerfile spec
  case generatorResult of
    Left generatorErrors -> return $ Left generatorErrors
    Right (FileDraftTemplateFd draft) -> do
      content <- compileAndRenderTemplate (TmplFD._srcPathInTmplDir draft) (fromMaybe (object []) (TmplFD._tmplData draft))
      return $ Right content
    Right _ -> error "Attempted to display Dockerfile, but it was not a Template FileDraft!"
