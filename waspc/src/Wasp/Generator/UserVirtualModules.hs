module Wasp.Generator.UserVirtualModules
  ( serverEnvValidationSchemaVMId,
    userPrismaSetupFnVMId,
    userOperationVMId,
    clientEnvValidationSchemaVMId,
    userClientSetupFnVMId,
    userClientRootComponentVMId,
  )
where

import Data.Maybe (fromJust)
import StrongPath (parseRelFileP, relfileP)
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.Generator.SdkGenerator.JsImport (VirtualModuleId)

serverEnvValidationSchemaVMId :: VirtualModuleId
serverEnvValidationSchemaVMId = [relfileP|virtual:wasp/user/server-env-validation-schema|]

userPrismaSetupFnVMId :: VirtualModuleId
userPrismaSetupFnVMId = [relfileP|virtual:wasp/user/prisma-setup-fn|]

userOperationVMId :: AS.Operation.Operation -> VirtualModuleId
userOperationVMId operation =
  fromJust . parseRelFileP $ "virtual:wasp/user/" ++ operationType ++ "/" ++ oprationName
  where
    oprationName = AS.Operation.getName operation
    operationType = case operation of
      QueryOp _ _ -> "query"
      ActionOp _ _ -> "action"

clientEnvValidationSchemaVMId :: VirtualModuleId
clientEnvValidationSchemaVMId = [relfileP|virtual:wasp/user/client-env-validation-schema|]

userClientSetupFnVMId :: VirtualModuleId
userClientSetupFnVMId = [relfileP|virtual:wasp/user/client-setup-fn|]

userClientRootComponentVMId :: VirtualModuleId
userClientRootComponentVMId = [relfileP|virtual:wasp/user/client-root-component|]
