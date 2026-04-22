module Wasp.Generator.UserVirtualModules
  ( userServerEnvSchemaVF,
    userPrismaSetupFnVF,
    userOperationVF,
    userClientEnvSchemaVF,
    userClientSetupFnVF,
    userClientRootComponentVF,
  )
where

import Data.Maybe (fromJust)
import StrongPath (parseRelFileP, relfileP)
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.JsImport (VirtualFile)

userServerEnvSchemaVF :: VirtualFile
userServerEnvSchemaVF = [relfileP|virtual:wasp/user/server-env-schema|]

userPrismaSetupFnVF :: VirtualFile
userPrismaSetupFnVF = [relfileP|virtual:wasp/user/prisma-setup-fn|]

userOperationVF :: AS.Operation.Operation -> VirtualFile
userOperationVF operation =
  fromJust . parseRelFileP $ "virtual:wasp/user/" ++ operationType ++ "/" ++ oprationName
  where
    oprationName = AS.Operation.getName operation
    operationType = case operation of
      QueryOp _ _ -> "query"
      ActionOp _ _ -> "action"

userClientEnvSchemaVF :: VirtualFile
userClientEnvSchemaVF = [relfileP|virtual:wasp/user/client-env-schema|]

userClientSetupFnVF :: VirtualFile
userClientSetupFnVF = [relfileP|virtual:wasp/user/client-setup-fn|]

userClientRootComponentVF :: VirtualFile
userClientRootComponentVF = [relfileP|virtual:wasp/user/client-root-component|]
