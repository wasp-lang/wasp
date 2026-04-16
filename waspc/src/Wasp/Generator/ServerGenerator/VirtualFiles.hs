module Wasp.Generator.ServerGenerator.VirtualFiles
  ( getUserVFData,
    userServerEnvSchemaVF,
    userPrismaSetupVF,
    userOperationVF,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe
  ( fromJust,
    maybeToList,
  )
import StrongPath
  ( Dir,
    Path,
    Posix,
    Rel,
    reldirP,
    relfileP,
    toFilePath,
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.JsImport (VirtualFile)

getUserVFData :: AppSpec -> [Aeson.Value]
getUserVFData spec =
  maybeToList (mkUserVFFromExtImport userServerEnvSchemaVF <$> maybeServerEnvSchema)
    ++ maybeToList (mkUserVFFromExtImport userPrismaSetupVF <$> maybePrismaSetupFn)
    ++ map mkOperationVFData allOperations
  where
    mkOperationVFData :: AS.Operation.Operation -> Aeson.Value
    mkOperationVFData operation =
      mkUserVFFromExtImport (userOperationVF operation) (AS.Operation.getFn operation)

    mkUserVFFromExtImport :: VirtualFile -> EI.ExtImport -> Aeson.Value
    mkUserVFFromExtImport vf extImport =
      let importJson = extImportToImportJson rollupDirToServerSrcDir (Just extImport)
       in object
            [ "virtualPath" .= toFilePath vf,
              "importJson" .= importJson
            ]

    rollupDirToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    rollupDirToServerSrcDir = [reldirP|src/|]

    maybeServerEnvSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    maybePrismaSetupFn = AS.App.db app >>= AS.Db.prismaSetupFn
    allOperations = AS.getOperations spec
    app = snd $ getApp spec

userServerEnvSchemaVF :: VirtualFile
userServerEnvSchemaVF = [relfileP|virtual:wasp/user-server-env-schema|]

userPrismaSetupVF :: VirtualFile
userPrismaSetupVF = [relfileP|virtual:wasp/user-prisma-setup|]

userOperationVF :: AS.Operation.Operation -> VirtualFile
userOperationVF operation =
  fromJust . SP.parseRelFileP $ "virtual:wasp/" ++ operationType ++ "/" ++ oprationName
  where
    oprationName = AS.Operation.getName operation
    operationType = case operation of
      QueryOp _ _ -> "query"
      ActionOp _ _ -> "action"
