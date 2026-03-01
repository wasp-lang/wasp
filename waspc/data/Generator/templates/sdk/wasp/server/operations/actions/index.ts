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
{=/ operations =}
{=# operations =}

// PRIVATE API
export type {= operationTypeName =} = typeof {= jsFn.importIdentifier =}

// PUBLIC API
{=! Assign entities to a variable to avoid TypeScript's excess property
    checking on object literals (module operations may declare more entities
    in their config than their function type requires). =}
const _{= operationName =}_entities = {
  {=# entities =}
  {= name =}: prisma.{= prismaIdentifier =},
  {=/ entities =}
}
{=# usesAuth =}
export const {= operationName =}: AuthenticatedOperationFor<{= operationTypeName =}> =
  createAuthenticatedOperation(
{=/ usesAuth =}
{=^ usesAuth =}
export const {= operationName =}: UnauthenticatedOperationFor<{= operationTypeName =}> =
  createUnauthenticatedOperation(
{=/ usesAuth =}
    {= jsFn.importIdentifier =},
    _{= operationName =}_entities,
  )
{=/ operations =}
