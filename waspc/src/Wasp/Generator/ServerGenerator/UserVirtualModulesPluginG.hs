module Wasp.Generator.ServerGenerator.UserVirtualModulesPluginG
  ( genUserVirtualModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe
  ( fromJust,
    maybeToList,
  )
import StrongPath
  ( relDirToPosix,
    relfile,
    toFilePath,
  )
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common (serverSrcDirInServerRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.UserVirtualModules
  ( VirtualModuleId,
    serverEnvValidationSchemaVMId,
    userOperationVMId,
    userPrismaSetupFnVMId,
  )

genUserVirtualModulesPlugin :: AppSpec -> Generator FileDraft
genUserVirtualModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      [relfile|src/plugins/userVirtualModules.js|]
      (Just $ object ["userVirtualModules" .= getServerUserVirtualModulesData spec])

getServerUserVirtualModulesData :: AppSpec -> [Aeson.Value]
getServerUserVirtualModulesData spec =
  maybeToList (mkVMImportData serverEnvValidationSchemaVMId <$> maybeServerEnvSchema)
    ++ maybeToList (mkVMImportData userPrismaSetupFnVMId <$> maybePrismaSetupFn)
    ++ map mkOperationVMImportData allOperations
  where
    mkOperationVMImportData :: AS.Operation.Operation -> Aeson.Value
    mkOperationVMImportData operation =
      mkVMImportData (userOperationVMId operation) (AS.Operation.getFn operation)

    mkVMImportData :: VirtualModuleId -> EI.ExtImport -> Aeson.Value
    mkVMImportData virtualModuleId extImport =
      object
        [ "virtualModuleId" .= toFilePath virtualModuleId,
          "importJson" .= importJson
        ]
      where
        importJson = extImportToImportJson importLocation (Just extImport)
        importLocation = fromJust $ relDirToPosix serverSrcDirInServerRootDir

    maybeServerEnvSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    maybePrismaSetupFn = AS.App.db app >>= AS.Db.prismaSetupFn
    allOperations = AS.getOperations spec
    app = snd $ getApp spec
