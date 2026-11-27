
import { prisma } from '../../index'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers'
import type { FromRegisterPath } from '../../../types/register'
import type {
  DeleteTask,
  CreateTask,
} from './types'
import { deleteTask as deleteTask_ext } from "virtual:wasp/user/tasks/operations"
import { createTask as createTask_ext } from "virtual:wasp/user/tasks/operations"

// PRIVATE API
export type RegisteredDeleteTask = FromRegisterPath<['operations', 'deleteTask'], DeleteTask>

// PUBLIC API
export const deleteTask: AuthenticatedOperationFor<RegisteredDeleteTask> =
  createAuthenticatedOperation<RegisteredDeleteTask>(
    () => deleteTask_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredCreateTask = FromRegisterPath<['operations', 'createTask'], CreateTask>

// PUBLIC API
export const createTask: AuthenticatedOperationFor<RegisteredCreateTask> =
  createAuthenticatedOperation<RegisteredCreateTask>(
    () => createTask_ext,
    {
      Task: prisma.task,
    },
  )
