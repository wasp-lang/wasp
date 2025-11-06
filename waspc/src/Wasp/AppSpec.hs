{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec
  ( AppSpec (..),
    Decl,
    getDecls,
    takeDecls,
    Ref,
    refName,
    getActions,
    getQueries,
    getOperations,
    getApis,
    getEntities,
    getPages,
    getRoutes,
    getJobs,
    resolveRef,
    asAbsWaspProjectDirFile,
    getApp,
    getApiNamespaces,
    getCruds,
    userNodeVersionRange,
  )
where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import StrongPath (Abs, Dir, File, File', Path', Rel, (</>))
import Wasp.AppSpec.Action (Action)
import Wasp.AppSpec.Api (Api)
import Wasp.AppSpec.ApiNamespace (ApiNamespace)
import Wasp.AppSpec.App (App)
import Wasp.AppSpec.ConfigFile (ConfigFileRelocator (..))
import Wasp.AppSpec.Core.Decl (Decl, takeDecls)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref, refName)
import Wasp.AppSpec.Crud (Crud)
import Wasp.AppSpec.Entity (Entity)
import qualified Wasp.AppSpec.ExternalFiles as ExternalFiles
import Wasp.AppSpec.Job (Job)
import Wasp.AppSpec.Operation (Operation)
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Page (Page)
import Wasp.AppSpec.Query (Query)
import Wasp.AppSpec.Route (Route)
import Wasp.Env (EnvVar)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson)
import Wasp.ExternalConfig.TsConfig (TsConfig)
import Wasp.Node.Version (oldestWaspSupportedNodeVersion)
import Wasp.Project.Common (SrcTsConfigFile, WaspProjectDir)
import Wasp.Project.Db.Migrations (DbMigrationsDir)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.SemanticVersion as SV

-- | AppSpec is the main/central intermediate representation (IR) of the whole Wasp compiler,
-- describing the web app specification with all the details needed to generate it.
-- It is standalone and de-coupled from other parts of the compiler and knows nothing about them,
-- instead other parts are using it: Analyzer produces AppSpec while Generator consumes it.
--
-- IMPORTANT: Do not change this data structure without updating the AppSpec in
-- packages/wasp-config/src/appSpec.ts. That module is a TypeScript mirror
-- implementation of AppSpec's FromJSON.
data AppSpec = AppSpec
  { -- | List of declarations like App, Page, Route, ... that describe the web app.
    decls :: [Decl],
    -- | Parsed Prisma schema file.
    prismaSchema :: Psl.Schema.Schema,
    -- | The contents of the package.json file found in the root directory of the wasp project.
    packageJson :: PackageJson,
    -- | Absolute path to the directory containing the wasp project.
    waspProjectDir :: Path' Abs (Dir WaspProjectDir),
    -- | List of external code files (they are referenced/used in the declarations).
    externalCodeFiles :: [ExternalFiles.CodeFile],
    externalPublicFiles :: [ExternalFiles.PublicFile],
    migrationsDir :: Maybe (Path' Abs (Dir DbMigrationsDir)),
    -- | Env variables to be provided to the server only during the development.
    devEnvVarsServer :: [EnvVar],
    -- | Env variables to be provided to the client only during the development.
    devEnvVarsClient :: [EnvVar],
    -- | If true, it means project is being compiled for production/deployment -> it is being "built".
    -- If false, it means project is being compiled for development purposes (e.g. "wasp start").
    isBuild :: Bool,
    -- | The contents of the optional user Dockerfile found in the root of the wasp project source.
    userDockerfileContents :: Maybe Text,
    -- | A list of paths to Tailwind specific config files and where to copy them.
    tailwindConfigFilesRelocators :: [ConfigFileRelocator],
    -- | Connection URL for a database used during development. If provided, generated app will
    -- make sure to use it when run in development mode.
    devDatabaseUrl :: Maybe String,
    customViteConfigPath :: Maybe (Path' (Rel WaspProjectDir) File'),
    srcTsConfigPath :: Path' (Rel WaspProjectDir) (File SrcTsConfigFile),
    srcTsConfig :: TsConfig
  }

-- TODO: Make this return "Named" declarations?
-- We would have something like NamedDecl or smth like that. Or at least have a @type Named@ or smth like that.
-- Or @WithName@ or just @Named@.
-- I like the best: `newtype Named a = Named (String, a)`
-- I created a github issue for it: https://github.com/wasp-lang/wasp/issues/426 .
getDecls :: (IsDecl a) => AppSpec -> [(String, a)]
getDecls = takeDecls . decls

getEntities :: AppSpec -> [(String, Entity)]
getEntities = getDecls

getQueries :: AppSpec -> [(String, Query)]
getQueries = getDecls

getActions :: AppSpec -> [(String, Action)]
getActions = getDecls

getOperations :: AppSpec -> [Operation]
getOperations spec =
  map (uncurry AS.Operation.QueryOp) (getQueries spec)
    <> map (uncurry AS.Operation.ActionOp) (getActions spec)

getApis :: AppSpec -> [(String, Api)]
getApis = getDecls

getApiNamespaces :: AppSpec -> [(String, ApiNamespace)]
getApiNamespaces = getDecls

getCruds :: AppSpec -> [(String, Crud)]
getCruds = getDecls

getPages :: AppSpec -> [(String, Page)]
getPages = getDecls

getRoutes :: AppSpec -> [(String, Route)]
getRoutes = getDecls

getJobs :: AppSpec -> [(String, Job)]
getJobs = getDecls

getApp :: [Decl] -> Maybe (String, App)
getApp dcls = case takeDecls @App dcls of
  [] -> Nothing
  apps -> Just $ head apps

resolveRef :: (IsDecl d) => AppSpec -> Ref d -> (String, d)
resolveRef spec ref =
  fromMaybe
    ( error $
        "Failed to resolve declaration reference: "
          ++ refName ref
          ++ "."
          ++ " This should never happen, as Analyzer should ensure all references in AppSpec are valid."
    )
    $ find ((== refName ref) . fst)
    $ getDecls spec

asAbsWaspProjectDirFile :: AppSpec -> Path' (Rel WaspProjectDir) File' -> Path' Abs File'
asAbsWaspProjectDirFile spec file = waspProjectDir spec </> file

-- This is the node version range that user expects his code to work on.
-- TODO: Once user will be able to specify package.json, extract this range from
--   `engines` field in package.json.
--   In the meantime, we determine it based on the oldest node version that Wasp supports.
userNodeVersionRange :: AppSpec -> SV.Range
userNodeVersionRange _ = SV.Range [SV.backwardsCompatibleWith oldestWaspSupportedNodeVersion]
