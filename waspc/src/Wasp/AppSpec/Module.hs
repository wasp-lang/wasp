{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Module
  ( ModuleSpec (..),
    EntityDeclaration (..),
    ModuleOperation (..),
    ModulePage (..),
    ModuleRoute (..),
    ModuleApi (..),
    ModuleApiNamespace (..),
    ModuleCrud (..),
    ModuleJob (..),
    ModuleDecl (..),
    EntityRef (..),
    parseDeclList,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport (ExtImport)

data EntityDeclaration = EntityDeclaration
  { edName :: String,
    edFields :: Map.Map String String
  }
  deriving (Show, Eq, Generic)

instance FromJSON EntityDeclaration where
  parseJSON = withObject "EntityDeclaration" $ \v ->
    EntityDeclaration
      <$> v .: "name"
      <*> v .: "fields"

-- | A reference to an entity, as serialized by the TS side: { name: "...", declType: "Entity" }
newtype EntityRef = EntityRef {erName :: String}
  deriving (Show, Eq, Generic)

instance FromJSON EntityRef where
  parseJSON = withObject "EntityRef" $ \v -> do
    EntityRef <$> v .: "name"

-- | A Decl wrapper matching the TS output: { declType: "...", declName: "...", declValue: ... }
data ModuleDecl a = ModuleDecl
  { mdDeclName :: String,
    mdDeclValue :: a
  }
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (ModuleDecl a) where
  parseJSON = withObject "ModuleDecl" $ \v ->
    ModuleDecl
      <$> v .: "declName"
      <*> v .: "declValue"

-- | Parse a list of decls, extracting name-value pairs.
parseDeclList :: [ModuleDecl a] -> [(String, a)]
parseDeclList = map (\d -> (mdDeclName d, mdDeclValue d))

data ModuleOperation = ModuleOperation
  { moFn :: ExtImport,
    moEntities :: Maybe [EntityRef],
    moAuth :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON ModuleOperation where
  parseJSON = withObject "ModuleOperation" $ \v ->
    ModuleOperation
      <$> v .: "fn"
      <*> v .:? "entities"
      <*> v .:? "auth"

data ModulePage = ModulePage
  { mpComponent :: ExtImport,
    mpAuthRequired :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON ModulePage where
  parseJSON = withObject "ModulePage" $ \v ->
    ModulePage
      <$> v .: "component"
      <*> v .:? "authRequired"

data ModuleRoute = ModuleRoute
  { mrPath :: String,
    mrTo :: Aeson.Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON ModuleRoute where
  parseJSON = withObject "ModuleRoute" $ \v ->
    ModuleRoute
      <$> v .: "path"
      <*> v .: "to"

data ModuleApi = ModuleApi
  { maFn :: ExtImport,
    maMiddlewareConfigFn :: Maybe ExtImport,
    maEntities :: Maybe [EntityRef],
    maHttpRoute :: Aeson.Value,
    maAuth :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON ModuleApi where
  parseJSON = withObject "ModuleApi" $ \v ->
    ModuleApi
      <$> v .: "fn"
      <*> v .:? "middlewareConfigFn"
      <*> v .:? "entities"
      <*> v .: "httpRoute"
      <*> v .:? "auth"

data ModuleApiNamespace = ModuleApiNamespace
  { manMiddlewareConfigFn :: ExtImport,
    manPath :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON ModuleApiNamespace where
  parseJSON = withObject "ModuleApiNamespace" $ \v ->
    ModuleApiNamespace
      <$> v .: "middlewareConfigFn"
      <*> v .: "path"

data ModuleCrud = ModuleCrud
  { mcEntity :: EntityRef,
    mcOperations :: Aeson.Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON ModuleCrud where
  parseJSON = withObject "ModuleCrud" $ \v ->
    ModuleCrud
      <$> v .: "entity"
      <*> v .: "operations"

data ModuleJob = ModuleJob
  { mjExecutor :: String,
    mjPerform :: Aeson.Value,
    mjSchedule :: Maybe Aeson.Value,
    mjEntities :: Maybe [EntityRef]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ModuleJob where
  parseJSON = withObject "ModuleJob" $ \v ->
    ModuleJob
      <$> v .: "executor"
      <*> v .: "perform"
      <*> v .:? "schedule"
      <*> v .:? "entities"

data ModuleSpec = ModuleSpec
  { msPackageName :: String,
    msEntities :: [EntityDeclaration],
    msRequiresAuth :: Bool,
    msQueries :: [(String, ModuleOperation)],
    msActions :: [(String, ModuleOperation)],
    msPages :: [(String, ModulePage)],
    msRoutes :: [(String, ModuleRoute)],
    msApis :: [(String, ModuleApi)],
    msApiNamespaces :: [(String, ModuleApiNamespace)],
    msCruds :: [(String, ModuleCrud)],
    msJobs :: [(String, ModuleJob)],
    msProvides :: Map.Map String Aeson.Value,
    msServerSetupFn :: Maybe ExtImport,
    msClientSetupFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Generic)

instance FromJSON ModuleSpec where
  parseJSON = withObject "ModuleSpec" $ \v -> do
    queryDecls <- v .: "queries"
    actionDecls <- v .: "actions"
    pageDecls <- v .: "pages"
    routeDecls <- v .: "routes"
    apiDecls <- v .: "apis"
    apiNamespaceDecls <- v .: "apiNamespaces"
    crudDecls <- v .: "cruds"
    jobDecls <- v .: "jobs"
    ModuleSpec
      <$> v .: "packageName"
      <*> v .: "entityDeclarations"
      <*> v .: "requiresAuth"
      <*> pure (parseDeclList queryDecls)
      <*> pure (parseDeclList actionDecls)
      <*> pure (parseDeclList pageDecls)
      <*> pure (parseDeclList routeDecls)
      <*> pure (parseDeclList apiDecls)
      <*> pure (parseDeclList apiNamespaceDecls)
      <*> pure (parseDeclList crudDecls)
      <*> pure (parseDeclList jobDecls)
      <*> v .: "provides"
      <*> v .:? "serverSetupFn"
      <*> v .:? "clientSetupFn"
