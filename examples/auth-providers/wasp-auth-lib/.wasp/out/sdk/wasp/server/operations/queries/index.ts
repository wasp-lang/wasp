
import { prisma } from '../../index'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers'
import type { FromRegisterPath } from '../../../types/register'
import type {
  GetMyTasks,
} from './types'
import { getMyTasks as getMyTasks_ext } from 'virtual:wasp/user/operations'

// PRIVATE API
export type RegisteredGetMyTasks = FromRegisterPath<['operations', 'getMyTasks'], GetMyTasks>

// PUBLIC API
export const getMyTasks: AuthenticatedOperationFor<RegisteredGetMyTasks> =
  createAuthenticatedOperation<RegisteredGetMyTasks>(
    () => getMyTasks_ext,
    {
      Task: prisma.task,
    },
  )

