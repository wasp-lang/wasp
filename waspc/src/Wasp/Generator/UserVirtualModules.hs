module Wasp.Generator.UserVirtualModules
  ( userServerEnvSchemaVMId,
    userPrismaSetupFnVMId,
    userOperationVMId,
    userClientEnvSchemaVMId,
    userClientSetupFnVMId,
    userClientRootComponentVMId,
    VirtualModuleId,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir', File', Path, Posix, Rel, parseRelFileP, relfileP)
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as AS.Operation

type VirtualModuleId = Path Posix (Rel Dir') File'

userServerEnvSchemaVMId :: VirtualModuleId
userServerEnvSchemaVMId = [relfileP|virtual:wasp/user/server-env-schema|]

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

userClientEnvSchemaVMId :: VirtualModuleId
userClientEnvSchemaVMId = [relfileP|virtual:wasp/user/client-env-schema|]

userClientSetupFnVMId :: VirtualModuleId
userClientSetupFnVMId = [relfileP|virtual:wasp/user/client-setup-fn|]

userClientRootComponentVMId :: VirtualModuleId
userClientRootComponentVMId = [relfileP|virtual:wasp/user/client-root-component|]
