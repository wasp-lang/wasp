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
import { getOperation } from '../operationsRegistry.js'
import type { Register } from 'wasp/types'
import type {
  {=# operations =}
  {= typeName =},
  {=/ operations =}
} from './types.js'

// PRIVATE API (exported so TypeScript preserves it in .d.ts instead of inlining
// and eagerly resolving the conditional before module augmentation can take effect)
export type _GetOperationType<K extends string, Default> =
  K extends keyof Register
    ? Register[K]
    : Default
{=# operations =}

// PRIVATE API
export type {= operationTypeName =} = _GetOperationType<'operation_{= operationName =}', {= typeName =}>

// PUBLIC API
{=# usesAuth =}
export const {= operationName =}: AuthenticatedOperationFor<{= operationTypeName =}> =
  createAuthenticatedOperation(
{=/ usesAuth =}
{=^ usesAuth =}
export const {= operationName =}: UnauthenticatedOperationFor<{= operationTypeName =}> =
  createUnauthenticatedOperation(
{=/ usesAuth =}
    getOperation('{= operationName =}') as {= operationTypeName =},
    {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    },
  )
{=/ operations =}
