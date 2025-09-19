
import { prisma } from 'wasp/server'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers.js'
import { foo as foo_ext } from 'wasp/src/server/queries/bar'

// PRIVATE API
export type MySpecialQuery_ext = typeof foo_ext

// PUBLIC API
export const mySpecialQuery: AuthenticatedOperationFor<MySpecialQuery_ext> =
  createAuthenticatedOperation(
    foo_ext,
    {
      User: prisma.user,
    },
  )

