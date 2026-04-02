{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for actions,
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
import type { FromOperationsRegistry } from 'wasp/types'
import type {
  {=# operations =}
  {= typeName =},
  {=/ operations =}
} from './types.js'
{=# operations =}
// @ts-expect-error
{=& jsFn.importStatement =}
{=/ operations =}
{=# operations =}

// PRIVATE API
export type {= operationTypeName =} = FromOperationsRegistry<'{= operationName =}', {= typeName =}>

// PUBLIC API
{=# usesAuth =}
export const {= operationName =}: AuthenticatedOperationFor<{= operationTypeName =}> =
  createAuthenticatedOperation<{= operationTypeName =}>(
{=/ usesAuth =}
{=^ usesAuth =}
export const {= operationName =}: UnauthenticatedOperationFor<{= operationTypeName =}> =
  createUnauthenticatedOperation<{= operationTypeName =}>(
{=/ usesAuth =}
    () => {= jsFn.importIdentifier =},
    {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    },
  )
{=/ operations =}
