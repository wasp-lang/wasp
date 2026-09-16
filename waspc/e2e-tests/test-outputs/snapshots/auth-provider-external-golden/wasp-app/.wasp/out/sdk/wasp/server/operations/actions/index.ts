
import { prisma } from '../../index'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers'
import type { FromRegisterPath } from '../../../types/register'
import type {
  CreateTask,
} from './types'
import { createTask as createTask_ext } from 'virtual:wasp/user/operations'

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
