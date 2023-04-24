{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.DockerGenerator
  ( genDockerFiles,
    genDockerfile,
    compileAndRenderDockerfile,
  )
where

import Data.Aeson (object, (.=))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import StrongPath (File, File', Path', Rel, relfile)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Generator.Common
  ( ProjectRootDir,
    ServerRootDir,
  )
import Wasp.Generator.DbGenerator.Common
  ( PrismaDbSchema,
    dbSchemaFileFromAppComponentDir,
    serverPrismaClientOutputDirEnv,
  )
import Wasp.Generator.FileDraft (FileDraft (..), createTemplateFileDraft)
import qualified Wasp.Generator.FileDraft.TemplateFileDraft as TmplFD
import Wasp.Generator.Monad (Generator, GeneratorError, runGenerator)
import Wasp.Generator.Templates (TemplatesDir, compileAndRenderTemplate)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (getEnvVarDefinition)
import Wasp.Util.NodeVersion (latestMajorNodeVersion)

genDockerFiles :: AppSpec -> Generator [FileDraft]
genDockerFiles spec = sequence [genDockerfile spec, genDockerignore spec]

-- TODO: Inject paths to server and db files/dirs, right now they are hardcoded in the templates.
genDockerfile :: AppSpec -> Generator FileDraft
genDockerfile spec = do
  let dbSchemaFileFromServerDir :: Path' (Rel ServerRootDir) (File PrismaDbSchema) = dbSchemaFileFromAppComponentDir
  return $
    createTemplateFileDraft
      ([relfile|Dockerfile|] :: Path' (Rel ProjectRootDir) File')
      ([relfile|Dockerfile|] :: Path' (Rel TemplatesDir) File')
      ( Just $
          object
            [ "usingPrisma" .= not (null $ AS.getDecls @AS.Entity.Entity spec),
              "serverPrismaClientOutputDirEnv" .= getEnvVarDefinition serverPrismaClientOutputDirEnv,
              "dbSchemaFileFromServerDir" .= SP.fromRelFile dbSchemaFileFromServerDir,
              "nodeMajorVersion" .= show (SV.major latestMajorNodeVersion),
              "userDockerfile" .= fromMaybe "" (AS.userDockerfileContents spec)
            ]
      )

genDockerignore :: AppSpec -> Generator FileDraft
genDockerignore _ =
  return $
    createTemplateFileDraft
      ([relfile|.dockerignore|] :: Path' (Rel ProjectRootDir) File')
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
