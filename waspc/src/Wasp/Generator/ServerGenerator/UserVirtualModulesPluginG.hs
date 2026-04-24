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
  ( VirtualFile,
    userOperationVF,
    userPrismaSetupFnVF,
    userServerEnvSchemaVF,
  )

genUserVirtualModulesPlugin :: AppSpec -> Generator FileDraft
genUserVirtualModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      [relfile|src/plugins/userVirtualModules.js|]
      (Just $ object ["userVirtualModules" .= getServerUserVirtualModulesData spec])

getServerUserVirtualModulesData :: AppSpec -> [Aeson.Value]
getServerUserVirtualModulesData spec =
  maybeToList (mkImportData userServerEnvSchemaVF <$> maybeServerEnvSchema)
    ++ maybeToList (mkImportData userPrismaSetupFnVF <$> maybePrismaSetupFn)
    ++ map mkOperationImportData allOperations
  where
    mkOperationImportData :: AS.Operation.Operation -> Aeson.Value
    mkOperationImportData operation =
      mkImportData (userOperationVF operation) (AS.Operation.getFn operation)

    mkImportData :: VirtualFile -> EI.ExtImport -> Aeson.Value
    mkImportData vf extImport =
      object
        [ "virtualPath" .= toFilePath vf,
          "importJson" .= importJson
        ]
      where
        importJson = extImportToImportJson importLocation (Just extImport)
        importLocation = fromJust $ relDirToPosix serverSrcDirInServerRootDir

    maybeServerEnvSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    maybePrismaSetupFn = AS.App.db app >>= AS.Db.prismaSetupFn
    allOperations = AS.getOperations spec
    app = snd $ getApp spec
