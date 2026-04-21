{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for queries,
          consider in the future if it is worth removing this duplication. =}

{=! TODO: This will generate multiple import statements even though they're
          importing symbols from the same file. We should improve our importing machinery
          to support multiple imports from the same file =}
import { prisma } from 'wasp/server'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  {=# isAuthEnabled =}
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
  {=/ isAuthEnabled =}
} from '../wrappers.js'
{=# operations =}
{=& jsFn.importStatement =}
import type { {= operationResolvedTypeName =} } from '{= jsFn.importPath =}'
{=/ operations =}
{=# operations =}

// PUBLIC API
{=# usesAuth =}
export const {= operationName =}: AuthenticatedOperationFor<{= operationResolvedTypeName =}> =
  createAuthenticatedOperation<{= operationResolvedTypeName =}>(
{=/ usesAuth =}
{=^ usesAuth =}
export const {= operationName =}: UnauthenticatedOperationFor<{= operationResolvedTypeName =}> =
  createUnauthenticatedOperation<{= operationResolvedTypeName =}>(
{=/ usesAuth =}
    () => {= jsFn.importIdentifier =},
    {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    },
  )
{=/ operations =}
