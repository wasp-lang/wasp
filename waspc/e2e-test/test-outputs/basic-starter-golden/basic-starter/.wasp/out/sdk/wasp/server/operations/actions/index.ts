
import { prisma } from 'wasp/server'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers.js'
import { createTask as createTask_ext } from 'wasp/src/tasks/actions'
import { updateTaskStatus as updateTaskStatus_ext } from 'wasp/src/tasks/actions'
import { deleteCompletedTasks as deleteCompletedTasks_ext } from 'wasp/src/tasks/actions'
import { createTag as createTag_ext } from 'wasp/src/tags/actions'

// PRIVATE API
export type CreateTask_ext = typeof createTask_ext

// PUBLIC API
export const createTask: AuthenticatedOperationFor<CreateTask_ext> =
  createAuthenticatedOperation(
    createTask_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type UpdateTaskStatus_ext = typeof updateTaskStatus_ext

// PUBLIC API
export const updateTaskStatus: AuthenticatedOperationFor<UpdateTaskStatus_ext> =
  createAuthenticatedOperation(
    updateTaskStatus_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type DeleteCompletedTasks_ext = typeof deleteCompletedTasks_ext

// PUBLIC API
export const deleteCompletedTasks: AuthenticatedOperationFor<DeleteCompletedTasks_ext> =
  createAuthenticatedOperation(
    deleteCompletedTasks_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type CreateTag_ext = typeof createTag_ext

// PUBLIC API
export const createTag: AuthenticatedOperationFor<CreateTag_ext> =
  createAuthenticatedOperation(
    createTag_ext,
    {
      Tag: prisma.tag,
    },
  )
