
import { prisma } from '../../index.js'
import { getServerOperation } from '../../runtime.js'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers.js'
import type { FromRegisterPath } from '../../../types/register.js'
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
} from './types.js'

// PRIVATE API
export type RegisteredCustomSignup = FromRegisterPath<['operations', 'customSignup'], CustomSignup>

// PUBLIC API
export const customSignup: AuthenticatedOperationFor<RegisteredCustomSignup> =
  createAuthenticatedOperation<RegisteredCustomSignup>(
    () => getServerOperation<RegisteredCustomSignup>('customSignup'),
    {
    },
  )

// PRIVATE API
export type RegisteredCreateTask = FromRegisterPath<['operations', 'createTask'], CreateTask>

// PUBLIC API
export const createTask: AuthenticatedOperationFor<RegisteredCreateTask> =
  createAuthenticatedOperation<RegisteredCreateTask>(
    () => getServerOperation<RegisteredCreateTask>('createTask'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredUpdateTaskIsDone = FromRegisterPath<['operations', 'updateTaskIsDone'], UpdateTaskIsDone>

// PUBLIC API
export const updateTaskIsDone: AuthenticatedOperationFor<RegisteredUpdateTaskIsDone> =
  createAuthenticatedOperation<RegisteredUpdateTaskIsDone>(
    () => getServerOperation<RegisteredUpdateTaskIsDone>('updateTaskIsDone'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredDeleteCompletedTasks = FromRegisterPath<['operations', 'deleteCompletedTasks'], DeleteCompletedTasks>

// PUBLIC API
export const deleteCompletedTasks: AuthenticatedOperationFor<RegisteredDeleteCompletedTasks> =
  createAuthenticatedOperation<RegisteredDeleteCompletedTasks>(
    () => getServerOperation<RegisteredDeleteCompletedTasks>('deleteCompletedTasks'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredToggleAllTasks = FromRegisterPath<['operations', 'toggleAllTasks'], ToggleAllTasks>

// PUBLIC API
export const toggleAllTasks: AuthenticatedOperationFor<RegisteredToggleAllTasks> =
  createAuthenticatedOperation<RegisteredToggleAllTasks>(
    () => getServerOperation<RegisteredToggleAllTasks>('toggleAllTasks'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredRequestUppercaseText = FromRegisterPath<['operations', 'requestUppercaseText'], RequestUppercaseText>

// PUBLIC API
export const requestUppercaseText: AuthenticatedOperationFor<RegisteredRequestUppercaseText> =
  createAuthenticatedOperation<RegisteredRequestUppercaseText>(
    () => getServerOperation<RegisteredRequestUppercaseText>('requestUppercaseText'),
    {
      UppercaseTextRequest: prisma.uppercaseTextRequest,
    },
  )

// PRIVATE API
export type RegisteredTestingAction = FromRegisterPath<['operations', 'testingAction'], TestingAction>

// PUBLIC API
export const testingAction: AuthenticatedOperationFor<RegisteredTestingAction> =
  createAuthenticatedOperation<RegisteredTestingAction>(
    () => getServerOperation<RegisteredTestingAction>('testingAction'),
    {
    },
  )

// PRIVATE API
export type RegisteredTaskToTaskUnspecified = FromRegisterPath<['operations', 'taskToTaskUnspecified'], TaskToTaskUnspecified>

// PUBLIC API
export const taskToTaskUnspecified: AuthenticatedOperationFor<RegisteredTaskToTaskUnspecified> =
  createAuthenticatedOperation<RegisteredTaskToTaskUnspecified>(
    () => getServerOperation<RegisteredTaskToTaskUnspecified>('taskToTaskUnspecified'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredTaskToTaskSatisfies = FromRegisterPath<['operations', 'taskToTaskSatisfies'], TaskToTaskSatisfies>

// PUBLIC API
export const taskToTaskSatisfies: AuthenticatedOperationFor<RegisteredTaskToTaskSatisfies> =
  createAuthenticatedOperation<RegisteredTaskToTaskSatisfies>(
    () => getServerOperation<RegisteredTaskToTaskSatisfies>('taskToTaskSatisfies'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredTaskToTaskSpecified = FromRegisterPath<['operations', 'taskToTaskSpecified'], TaskToTaskSpecified>

// PUBLIC API
export const taskToTaskSpecified: AuthenticatedOperationFor<RegisteredTaskToTaskSpecified> =
  createAuthenticatedOperation<RegisteredTaskToTaskSpecified>(
    () => getServerOperation<RegisteredTaskToTaskSpecified>('taskToTaskSpecified'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredVoidToStringAuth = FromRegisterPath<['operations', 'voidToStringAuth'], VoidToStringAuth>

// PUBLIC API
export const voidToStringAuth: AuthenticatedOperationFor<RegisteredVoidToStringAuth> =
  createAuthenticatedOperation<RegisteredVoidToStringAuth>(
    () => getServerOperation<RegisteredVoidToStringAuth>('voidToStringAuth'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredVoidToStringNoAuth = FromRegisterPath<['operations', 'voidToStringNoAuth'], VoidToStringNoAuth>

// PUBLIC API
export const voidToStringNoAuth: UnauthenticatedOperationFor<RegisteredVoidToStringNoAuth> =
  createUnauthenticatedOperation<RegisteredVoidToStringNoAuth>(
    () => getServerOperation<RegisteredVoidToStringNoAuth>('voidToStringNoAuth'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredUnspecifiedToNumber = FromRegisterPath<['operations', 'unspecifiedToNumber'], UnspecifiedToNumber>

// PUBLIC API
export const unspecifiedToNumber: AuthenticatedOperationFor<RegisteredUnspecifiedToNumber> =
  createAuthenticatedOperation<RegisteredUnspecifiedToNumber>(
    () => getServerOperation<RegisteredUnspecifiedToNumber>('unspecifiedToNumber'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredBoolToStringAuth = FromRegisterPath<['operations', 'boolToStringAuth'], BoolToStringAuth>

// PUBLIC API
export const boolToStringAuth: AuthenticatedOperationFor<RegisteredBoolToStringAuth> =
  createAuthenticatedOperation<RegisteredBoolToStringAuth>(
    () => getServerOperation<RegisteredBoolToStringAuth>('boolToStringAuth'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredBoolToStringNoAuth = FromRegisterPath<['operations', 'boolToStringNoAuth'], BoolToStringNoAuth>

// PUBLIC API
export const boolToStringNoAuth: UnauthenticatedOperationFor<RegisteredBoolToStringNoAuth> =
  createUnauthenticatedOperation<RegisteredBoolToStringNoAuth>(
    () => getServerOperation<RegisteredBoolToStringNoAuth>('boolToStringNoAuth'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredBoolToVoidNoAuth = FromRegisterPath<['operations', 'boolToVoidNoAuth'], BoolToVoidNoAuth>

// PUBLIC API
export const boolToVoidNoAuth: UnauthenticatedOperationFor<RegisteredBoolToVoidNoAuth> =
  createUnauthenticatedOperation<RegisteredBoolToVoidNoAuth>(
    () => getServerOperation<RegisteredBoolToVoidNoAuth>('boolToVoidNoAuth'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredBoolToVoidAuth = FromRegisterPath<['operations', 'boolToVoidAuth'], BoolToVoidAuth>

// PUBLIC API
export const boolToVoidAuth: AuthenticatedOperationFor<RegisteredBoolToVoidAuth> =
  createAuthenticatedOperation<RegisteredBoolToVoidAuth>(
    () => getServerOperation<RegisteredBoolToVoidAuth>('boolToVoidAuth'),
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RegisteredJsActionWithArgs = FromRegisterPath<['operations', 'jsActionWithArgs'], JsActionWithArgs>

// PUBLIC API
export const jsActionWithArgs: AuthenticatedOperationFor<RegisteredJsActionWithArgs> =
  createAuthenticatedOperation<RegisteredJsActionWithArgs>(
    () => getServerOperation<RegisteredJsActionWithArgs>('jsActionWithArgs'),
    {
      Task: prisma.task,
    },
  )
