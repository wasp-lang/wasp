{-# LANGUAGE LambdaCase #-}

module Wasp.Generator.SdkGenerator.VirtualUserModules
  ( VirtualUserModule (..),
    getClientVirtualUserModules,
    getServerVirtualUserModules,
    getVirtualUserModuleId,
    getVirtualUserModuleJsImportPath,
    getVirtualUserModuleExportName,
    getDeclaredTypeExpression,
    isDefaultExport,
    mkVirtualUserModulePluginData,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (maybeToList)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.SdkGenerator.Common (getRegisteredOperationTypeName)
import Wasp.JsImport (getJsImportPathStringFromPath)

-- | Virtual user modules are virtual modules pointing to the user's project files.
--
-- For a virtual user module to work, we need to:
--   1. Register it at the client or server bundler plugin (whichever it is
--      supposed to end up in), which resolves its module ID to a user file.
--   2. Generate an ambient module declaration for it in 
--      @wasp-user-virtual-modules.d.ts@.
data VirtualUserModule = VirtualUserModule
  { extImport :: EI.ExtImport,
    -- | Path, relative to the SDK root, of the module declaring 'declaredTypeName'.
    declaredTypeModulePath :: String,
    -- | Type the SDK expects this module's export to have.
    declaredTypeName :: String
  }

-- | Virtual user modules that end up in the client bundle.
getClientVirtualUserModules :: AppSpec -> [VirtualUserModule]
getClientVirtualUserModules spec =
  maybeToList $ mkEnvValidationSchemaModule <$> maybeClientEnvValidationSchema
  where
    mkEnvValidationSchemaModule extImport' =
      VirtualUserModule extImport' "./client/env/schema" "RegisteredClientEnvValidationSchema"

    maybeClientEnvValidationSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    app = snd $ getApp spec

-- | Virtual user modules that end up in the server bundle.
getServerVirtualUserModules :: AppSpec -> [VirtualUserModule]
getServerVirtualUserModules spec =
  maybeToList (mkEnvValidationSchemaModule <$> maybeServerEnvValidationSchema)
    ++ maybeToList (mkPrismaSetupFnModule <$> maybePrismaSetupFn)
    ++ map mkOperationModule (AS.getOperations spec)
  where
    mkEnvValidationSchemaModule extImport' =
      VirtualUserModule extImport' "./server/env" "RegisteredServerEnvValidationSchema"

    mkPrismaSetupFnModule extImport' =
      VirtualUserModule extImport' "./server/dbClient" "RegisteredPrismaSetupFn"

    mkOperationModule operation =
      VirtualUserModule
        (AS.Operation.getFn operation)
        (getOperationsIndexModulePath operation)
        (getRegisteredOperationTypeName operation)

    getOperationsIndexModulePath = \case
      AS.Operation.QueryOp _ _ -> "./server/operations/queries/index"
      AS.Operation.ActionOp _ _ -> "./server/operations/actions/index"

    maybeServerEnvValidationSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    maybePrismaSetupFn = AS.App.db app >>= AS.Db.prismaSetupFn
    app = snd $ getApp spec

getVirtualUserModuleId :: VirtualUserModule -> String
getVirtualUserModuleId =
  getJsImportPathStringFromPath . getVirtualUserModuleJsImportPath . EI.path . extImport

-- | Specifier the SDK imports a user module through, e.g. @virtual:wasp/user/queries.ts@.
getVirtualUserModuleJsImportPath :: Path Posix (Rel SourceExternalCodeDir) File' -> JsImportPath
getVirtualUserModuleJsImportPath userDefinedPathInExtSrcDir =
  RawImportName $ "virtual:wasp/user/" ++ SP.fromRelFileP userDefinedPathInExtSrcDir

-- | Name under which the user's module exports the value.
getVirtualUserModuleExportName :: VirtualUserModule -> String
getVirtualUserModuleExportName virtualUserModule = case EI.name $ extImport virtualUserModule of
  EI.ExtImportModule name -> name
  EI.ExtImportField name -> name

-- | Type the SDK expects the module's export to have, written as an inline import
-- type (e.g. @import("./server/env").RegisteredServerEnvValidationSchema@).
--
-- It has to be inline because ambient module declaration can't reach another
-- module through a relative import statement (TS2439).
--
-- @skipLibCheck@ tsconfig flag hides that error, so writing it as an import
-- statement instead would silently type every declared export as @any@.
getDeclaredTypeExpression :: VirtualUserModule -> String
getDeclaredTypeExpression virtualUserModule =
  "import(\"" ++ declaredTypeModulePath virtualUserModule ++ "\")." ++ declaredTypeName virtualUserModule

isDefaultExport :: VirtualUserModule -> Bool
isDefaultExport virtualUserModule = case EI.name $ extImport virtualUserModule of
  EI.ExtImportModule _ -> True
  EI.ExtImportField _ -> False

-- | Data for one entry of a bundler plugin's virtual module ID to user file map.
mkVirtualUserModulePluginData ::
  (EI.ExtImport -> Aeson.Value) ->
  VirtualUserModule ->
  Aeson.Value
mkVirtualUserModulePluginData extImportToImportJson virtualUserModule =
  object
    [ "virtualModuleId" .= getVirtualUserModuleId virtualUserModule,
      "importJson" .= extImportToImportJson (extImport virtualUserModule)
    ]
