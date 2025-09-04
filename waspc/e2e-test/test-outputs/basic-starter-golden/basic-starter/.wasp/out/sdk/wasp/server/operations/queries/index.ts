
import { prisma } from 'wasp/server'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers.js'
import { getTasks as getTasks_ext } from 'wasp/src/tasks/queries'
import { getTags as getTags_ext } from 'wasp/src/tags/queries'

// PRIVATE API
export type GetTasks_ext = typeof getTasks_ext

// PUBLIC API
export const getTasks: AuthenticatedOperationFor<GetTasks_ext> =
  createAuthenticatedOperation(
    getTasks_ext,
    {
      Task: prisma.task,
      Tag: prisma.tag,
    },
  )


// PRIVATE API
export type GetTags_ext = typeof getTags_ext

// PUBLIC API
export const getTags: AuthenticatedOperationFor<GetTags_ext> =
  createAuthenticatedOperation(
    getTags_ext,
    {
      Tag: prisma.tag,
    },
  )

