module Wasp.Generator.UserVirtualModules
  ( serverEnvValidationSchemaVMId,
    userPrismaSetupFnVMId,
    userOperationVMId,
    clientEnvValidationSchemaVMId,
    userClientSetupFnVMId,
    userClientRootComponentVMId,
    UserVirtualModuleId,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir', File', Path, Posix, Rel, parseRelFileP, relfileP)
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as AS.Operation

type UserVirtualModuleId = Path Posix (Rel Dir') File'

serverEnvValidationSchemaVMId :: UserVirtualModuleId
serverEnvValidationSchemaVMId = [relfileP|virtual:wasp/user/server-env-validation-schema|]

userPrismaSetupFnVMId :: UserVirtualModuleId
userPrismaSetupFnVMId = [relfileP|virtual:wasp/user/prisma-setup-fn|]

userOperationVMId :: AS.Operation.Operation -> UserVirtualModuleId
userOperationVMId operation =
  fromJust . parseRelFileP $ "virtual:wasp/user/" ++ operationType ++ "/" ++ oprationName
  where
    oprationName = AS.Operation.getName operation
    operationType = case operation of
      QueryOp _ _ -> "query"
      ActionOp _ _ -> "action"

clientEnvValidationSchemaVMId :: UserVirtualModuleId
clientEnvValidationSchemaVMId = [relfileP|virtual:wasp/user/client-env-validation-schema|]

userClientSetupFnVMId :: UserVirtualModuleId
userClientSetupFnVMId = [relfileP|virtual:wasp/user/client-setup-fn|]

userClientRootComponentVMId :: UserVirtualModuleId
userClientRootComponentVMId = [relfileP|virtual:wasp/user/client-root-component|]
