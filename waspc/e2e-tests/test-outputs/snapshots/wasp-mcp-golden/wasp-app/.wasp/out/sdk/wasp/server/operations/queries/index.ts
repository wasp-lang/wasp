
import { prisma } from '../../index'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers'
import type { FromRegisterPath } from '../../../types/register'
import type {
  GetTasks,
  GetTaskStats,
} from './types'
import { getTasks as getTasks_ext } from "virtual:wasp/user/tasks/operations"
import { getTaskStats as getTaskStats_ext } from "virtual:wasp/user/tasks/operations"

// PRIVATE API
export type RegisteredGetTasks = FromRegisterPath<['operations', 'getTasks'], GetTasks>

// PUBLIC API
export const getTasks: AuthenticatedOperationFor<RegisteredGetTasks> =
  createAuthenticatedOperation<RegisteredGetTasks>(
    () => getTasks_ext,
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type RegisteredGetTaskStats = FromRegisterPath<['operations', 'getTaskStats'], GetTaskStats>

// PUBLIC API
export const getTaskStats: AuthenticatedOperationFor<RegisteredGetTaskStats> =
  createAuthenticatedOperation<RegisteredGetTaskStats>(
    () => getTaskStats_ext,
    {
      Task: prisma.task,
    },
  )

