module Wasp.Generator.ServerGenerator.DevEnv
  ( prepareDevEnv,
    makeDevEnvFileDraft, -- Exported for testing.
  )
where

import StrongPath (relfile, (</>))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Env (EnvVar, envVarsToDotEnvContent)
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir)
import Wasp.Project.Db (databaseUrlEnvVarName, makeDevDatabaseUrl)

prepareDevEnv :: AS.AppSpec -> IO [FileDraft]
prepareDevEnv spec
  | AS.isProduction spec = return []
  | otherwise = do
      databaseUrl <- makeDevDatabaseUrl spec.waspProjectDir (ASV.getValidDbSystem spec) spec.decls
      return [makeDevEnvFileDraft spec.devEnvVarsServer databaseUrl]

makeDevEnvFileDraft :: [EnvVar] -> Maybe String -> FileDraft
makeDevEnvFileDraft userEnvVars databaseUrl =
  createTextFileDraft
    (serverRootDirInGeneratedAppDir </> [relfile|.env|])
    (envVarsToDotEnvContent envVars)
  where
    envVars = waspEnvVars ++ userEnvVars
    waspEnvVars = case databaseUrl of
      Just url | not isThereCustomDbUrl -> [(databaseUrlEnvVarName, url)]
      _ -> []
    isThereCustomDbUrl = any ((== databaseUrlEnvVarName) . fst) userEnvVars
