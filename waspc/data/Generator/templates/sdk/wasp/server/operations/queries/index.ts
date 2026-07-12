{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for actions,
          consider in the future if it is worth removing this duplication. =}

{=! TODO: This will generate multiple import statements even though they're
          importing symbols from the same file. We should improve our importing machinery
          to support multiple imports from the same file =}
import { prisma } from '../../index'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  {=# isAuthEnabled =}
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
  {=/ isAuthEnabled =}
} from '../wrappers'
import type { OperationFromRegister } from '../register'
import type {
  {=# operations =}
  {= genericOperationDefinitionTypeName =},
  {=/ operations =}
} from './types'
{=# operations =}
{=& jsFn.importStatement =}
{=/ operations =}
{=# operations =}

// PRIVATE API
export type Registered{= genericOperationDefinitionTypeName =} = OperationFromRegister<'{= operationName =}', {= genericOperationDefinitionTypeName =}>

// PUBLIC API
{=# usesAuth =}
export const {= operationName =}: AuthenticatedOperationFor<Registered{= genericOperationDefinitionTypeName =}> =
  createAuthenticatedOperation<Registered{= genericOperationDefinitionTypeName =}>(
{=/ usesAuth =}
{=^ usesAuth =}
export const {= operationName =}: UnauthenticatedOperationFor<Registered{= genericOperationDefinitionTypeName =}> =
  createUnauthenticatedOperation<Registered{= genericOperationDefinitionTypeName =}>(
{=/ usesAuth =}
    {= jsFn.importIdentifier =},
    {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    },
  )

{=/ operations =}
