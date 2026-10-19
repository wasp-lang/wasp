{-# LANGUAGE TupleSections #-}

module Wasp.Project.Db.Runtime
  ( resolveDevDbEnvVars,
  )
where

import Data.Maybe (isJust, maybeToList)
import System.Environment (lookupEnv)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Env (EnvVar)
import Wasp.Project.Db (databaseUrlEnvVarName, makeDevDatabaseUrl)

resolveDevDbEnvVars :: AS.AppSpec -> IO [EnvVar]
resolveDevDbEnvVars spec
  | AS.isProduction spec = return []
  | otherwise = do
      environmentUrl <- lookupEnv databaseUrlEnvVarName
      if isJust environmentUrl || hasServerUrl
        then return []
        else do
          databaseUrl <- makeDevDatabaseUrl spec.waspProjectDir (ASV.getValidDbSystem spec) spec.decls
          return $ maybeToList $ (databaseUrlEnvVarName,) <$> databaseUrl
  where
    hasServerUrl = any ((== databaseUrlEnvVarName) . fst) spec.devEnvVarsServer
