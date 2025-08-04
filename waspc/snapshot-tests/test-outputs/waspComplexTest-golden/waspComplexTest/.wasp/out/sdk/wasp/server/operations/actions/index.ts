
import { prisma } from 'wasp/server'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers.js'
import { foo as foo_ext } from 'wasp/src/server/actions/bar'

// PRIVATE API
export type MySpecialAction_ext = typeof foo_ext

// PUBLIC API
export const mySpecialAction: AuthenticatedOperationFor<MySpecialAction_ext> =
  createAuthenticatedOperation(
    foo_ext,
    {
      User: prisma.user,
    },
  )
