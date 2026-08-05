
import { prisma } from '../../index'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers'
import type { FromRegisterPath } from '../../../types/register'
import type {
  CustomSignup,
  CreateTask,
  UpdateTaskIsDone,
  DeleteCompletedTasks,
  ToggleAllTasks,
  RequestUppercaseText,
  TestingAction,
  TaskToTaskUnspecified,
  TaskToTaskSatisfies,
  TaskToTaskSpecified,
  VoidToStringAuth,
  VoidToStringNoAuth,
  UnspecifiedToNumber,
  BoolToStringAuth,
  BoolToStringNoAuth,
  BoolToVoidNoAuth,
  BoolToVoidAuth,
  JsActionWithArgs,
} from './types'
import { customSignup as customSignup_ext } from 'virtual:wasp/user/features/auth/customSignup'
import { createTask as createTask_ext } from 'virtual:wasp/user/features/operations/actions'
import { updateTaskIsDone as updateTaskIsDone_ext } from 'virtual:wasp/user/features/operations/actions'
import { deleteCompletedTasks as deleteCompletedTasks_ext } from 'virtual:wasp/user/features/operations/actions'
import { toggleAllTasks as toggleAllTasks_ext } from 'virtual:wasp/user/features/operations/actions'
import { requestUppercaseText as requestUppercaseText_ext } from 'virtual:wasp/user/features/jobs/uppercaseText'
import { testingAction as testingAction_ext } from 'virtual:wasp/user/rpcTests/operations/server'
import { taskToTaskUnspecified as taskToTaskUnspecified_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { taskToTaskSatisfies as taskToTaskSatisfies_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { taskToTaskSpecified as taskToTaskSpecified_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { voidToStringAuth as voidToStringAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { voidToStringNoAuth as voidToStringNoAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { unspecifiedToNumber as unspecifiedToNumber_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { boolToStringAuth as boolToStringAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { boolToStringNoAuth as boolToStringNoAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { boolToVoidNoAuth as boolToVoidNoAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { boolToVoidAuth as boolToVoidAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { jsActionWithArgs as jsActionWithArgs_ext } from 'virtual:wasp/user/rpcTests/operations/jsDefinitions'

// PRIVATE API
export type RegisteredCustomSignup = FromRegisterPath<['operations', 'customSignup'], CustomSignup>

// PUBLIC API
export const customSignup: AuthenticatedOperationFor<RegisteredCustomSignup> =
  createAuthenticatedOperation<RegisteredCustomSignup>(
    () => customSignup_ext,
    {
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

// PRIVATE API
export type RegisteredUpdateTaskIsDone = FromRegisterPath<['operations', 'updateTaskIsDone'], UpdateTaskIsDone>

// PUBLIC API
export const updateTaskIsDone: AuthenticatedOperationFor<RegisteredUpdateTaskIsDone> =
  createAuthenticatedOperation<RegisteredUpdateTaskIsDone>(
    () => updateTaskIsDone_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredDeleteCompletedTasks = FromRegisterPath<['operations', 'deleteCompletedTasks'], DeleteCompletedTasks>

// PUBLIC API
export const deleteCompletedTasks: AuthenticatedOperationFor<RegisteredDeleteCompletedTasks> =
  createAuthenticatedOperation<RegisteredDeleteCompletedTasks>(
    () => deleteCompletedTasks_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredToggleAllTasks = FromRegisterPath<['operations', 'toggleAllTasks'], ToggleAllTasks>

// PUBLIC API
export const toggleAllTasks: AuthenticatedOperationFor<RegisteredToggleAllTasks> =
  createAuthenticatedOperation<RegisteredToggleAllTasks>(
    () => toggleAllTasks_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredRequestUppercaseText = FromRegisterPath<['operations', 'requestUppercaseText'], RequestUppercaseText>

// PUBLIC API
export const requestUppercaseText: AuthenticatedOperationFor<RegisteredRequestUppercaseText> =
  createAuthenticatedOperation<RegisteredRequestUppercaseText>(
    () => requestUppercaseText_ext,
    {
      UppercaseTextRequest: prisma.uppercaseTextRequest,
    },
  )

// PRIVATE API
export type RegisteredTestingAction = FromRegisterPath<['operations', 'testingAction'], TestingAction>

// PUBLIC API
export const testingAction: AuthenticatedOperationFor<RegisteredTestingAction> =
  createAuthenticatedOperation<RegisteredTestingAction>(
    () => testingAction_ext,
    {
    },
  )

// PRIVATE API
export type RegisteredTaskToTaskUnspecified = FromRegisterPath<['operations', 'taskToTaskUnspecified'], TaskToTaskUnspecified>

// PUBLIC API
export const taskToTaskUnspecified: AuthenticatedOperationFor<RegisteredTaskToTaskUnspecified> =
  createAuthenticatedOperation<RegisteredTaskToTaskUnspecified>(
    () => taskToTaskUnspecified_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredTaskToTaskSatisfies = FromRegisterPath<['operations', 'taskToTaskSatisfies'], TaskToTaskSatisfies>

// PUBLIC API
export const taskToTaskSatisfies: AuthenticatedOperationFor<RegisteredTaskToTaskSatisfies> =
  createAuthenticatedOperation<RegisteredTaskToTaskSatisfies>(
    () => taskToTaskSatisfies_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredTaskToTaskSpecified = FromRegisterPath<['operations', 'taskToTaskSpecified'], TaskToTaskSpecified>

// PUBLIC API
export const taskToTaskSpecified: AuthenticatedOperationFor<RegisteredTaskToTaskSpecified> =
  createAuthenticatedOperation<RegisteredTaskToTaskSpecified>(
    () => taskToTaskSpecified_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredVoidToStringAuth = FromRegisterPath<['operations', 'voidToStringAuth'], VoidToStringAuth>

// PUBLIC API
export const voidToStringAuth: AuthenticatedOperationFor<RegisteredVoidToStringAuth> =
  createAuthenticatedOperation<RegisteredVoidToStringAuth>(
    () => voidToStringAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredVoidToStringNoAuth = FromRegisterPath<['operations', 'voidToStringNoAuth'], VoidToStringNoAuth>

// PUBLIC API
export const voidToStringNoAuth: UnauthenticatedOperationFor<RegisteredVoidToStringNoAuth> =
  createUnauthenticatedOperation<RegisteredVoidToStringNoAuth>(
    () => voidToStringNoAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredUnspecifiedToNumber = FromRegisterPath<['operations', 'unspecifiedToNumber'], UnspecifiedToNumber>

// PUBLIC API
export const unspecifiedToNumber: AuthenticatedOperationFor<RegisteredUnspecifiedToNumber> =
  createAuthenticatedOperation<RegisteredUnspecifiedToNumber>(
    () => unspecifiedToNumber_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredBoolToStringAuth = FromRegisterPath<['operations', 'boolToStringAuth'], BoolToStringAuth>

// PUBLIC API
export const boolToStringAuth: AuthenticatedOperationFor<RegisteredBoolToStringAuth> =
  createAuthenticatedOperation<RegisteredBoolToStringAuth>(
    () => boolToStringAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredBoolToStringNoAuth = FromRegisterPath<['operations', 'boolToStringNoAuth'], BoolToStringNoAuth>

// PUBLIC API
export const boolToStringNoAuth: UnauthenticatedOperationFor<RegisteredBoolToStringNoAuth> =
  createUnauthenticatedOperation<RegisteredBoolToStringNoAuth>(
    () => boolToStringNoAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredBoolToVoidNoAuth = FromRegisterPath<['operations', 'boolToVoidNoAuth'], BoolToVoidNoAuth>

// PUBLIC API
export const boolToVoidNoAuth: UnauthenticatedOperationFor<RegisteredBoolToVoidNoAuth> =
  createUnauthenticatedOperation<RegisteredBoolToVoidNoAuth>(
    () => boolToVoidNoAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredBoolToVoidAuth = FromRegisterPath<['operations', 'boolToVoidAuth'], BoolToVoidAuth>

// PUBLIC API
export const boolToVoidAuth: AuthenticatedOperationFor<RegisteredBoolToVoidAuth> =
  createAuthenticatedOperation<RegisteredBoolToVoidAuth>(
    () => boolToVoidAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredJsActionWithArgs = FromRegisterPath<['operations', 'jsActionWithArgs'], JsActionWithArgs>

// PUBLIC API
export const jsActionWithArgs: AuthenticatedOperationFor<RegisteredJsActionWithArgs> =
  createAuthenticatedOperation<RegisteredJsActionWithArgs>(
    () => jsActionWithArgs_ext,
    {
      Task: prisma.task,
    },
  )
