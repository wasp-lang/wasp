{{={= =}=}}
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
{=& jsFn.importStatement =}
{=/ operations =}
{=# operations =}

// PRIVATE API
export type {= operationTypeName =} = FromOperationsRegistry<'{= operationName =}', {= typeName =}>

// PUBLIC API
{=# usesAuth =}
export const {= operationName =}: AuthenticatedOperationFor<{= operationTypeName =}> =
  createAuthenticatedOperation(
{=/ usesAuth =}
{=^ usesAuth =}
export const {= operationName =}: UnauthenticatedOperationFor<{= operationTypeName =}> =
  createUnauthenticatedOperation(
{=/ usesAuth =}
    ((...args: any[]) => {= jsFn.importIdentifier =}(...args)),
    {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    },
  )
{=/ operations =}
