module Wasp.Generator.ServerGenerator.WaspVirtualUserModulesPluginG
  ( genWaspVirtualUserModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe
  ( fromJust,
    maybeToList,
  )
import StrongPath
  ( File',
    Path,
    Posix,
    Rel,
    relDirToPosix,
    relfile,
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getVirtualUserModuleJsImportPath)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common (serverSrcDirInServerRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.JsImport (getJsImportPathStringFromPath)

genWaspVirtualUserModulesPlugin :: AppSpec -> Generator FileDraft
genWaspVirtualUserModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      [relfile|src/plugins/waspVirtualUserModules.js|]
      (Just $ object ["virtualUserModules" .= getServerVirtualUserModulesData spec])

getServerVirtualUserModulesData :: AppSpec -> [Aeson.Value]
getServerVirtualUserModulesData spec =
  maybeToList (mkVMImportData <$> maybeServerEnvSchema)
    ++ maybeToList (mkVMImportData <$> maybePrismaSetupFn)
    ++ map mkOperationVMImportData allOperations
  where
    mkOperationVMImportData :: AS.Operation.Operation -> Aeson.Value
    mkOperationVMImportData operation =
      mkVMImportData (AS.Operation.getFn operation)

    mkVMImportData :: EI.ExtImport -> Aeson.Value
    mkVMImportData extImport =
      object
        [ "virtualModuleId" .= virtualModuleId,
          "importJson" .= importJson
        ]
      where
        importJson = extImportToImportJson importLocation (Just extImport)
        importLocation = fromJust $ relDirToPosix serverSrcDirInServerRootDir

        virtualModuleId = getJsImportPathStringFromPath $ getVirtualUserModuleJsImportPath userDefinedPathInExtSrcDir
        userDefinedPathInExtSrcDir = SP.castRel $ EI.path extImport :: Path Posix (Rel SourceExternalCodeDir) File'

    maybeServerEnvSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    maybePrismaSetupFn = AS.App.db app >>= AS.Db.prismaSetupFn
    allOperations = AS.getOperations spec
    app = snd $ getApp spec
