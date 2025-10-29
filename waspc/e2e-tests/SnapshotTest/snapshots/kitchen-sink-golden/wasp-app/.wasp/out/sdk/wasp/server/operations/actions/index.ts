
import { prisma } from 'wasp/server'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers.js'
import { signup as signup_ext } from 'wasp/src/features/auth/customSignup'
import { createTask as createTask_ext } from 'wasp/src/features/operations/actions'
import { updateTaskIsDone as updateTaskIsDone_ext } from 'wasp/src/features/operations/actions'
import { deleteCompletedTasks as deleteCompletedTasks_ext } from 'wasp/src/features/operations/actions'
import { toggleAllTasks as toggleAllTasks_ext } from 'wasp/src/features/operations/actions'
import { requestUppercaseText as requestUppercaseText_ext } from 'wasp/src/features/jobs/uppercaseText'
import { testingAction as testingAction_ext } from 'wasp/src/rpcTests/operations/server'
import { taskToTaskUnspecified as taskToTaskUnspecified_ext } from 'wasp/src/rpcTests/operations/definitions'
import { taskToTaskSatisfies as taskToTaskSatisfies_ext } from 'wasp/src/rpcTests/operations/definitions'
import { taskToTaskSpecified as taskToTaskSpecified_ext } from 'wasp/src/rpcTests/operations/definitions'
import { voidToStringAuth as voidToStringAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { voidToStringNoAuth as voidToStringNoAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { unspecifiedToNumber as unspecifiedToNumber_ext } from 'wasp/src/rpcTests/operations/definitions'
import { boolToStringAuth as boolToStringAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { boolToStringNoAuth as boolToStringNoAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { boolToVoidNoAuth as boolToVoidNoAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { boolToVoidAuth as boolToVoidAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { jsActionWithArgs as jsActionWithArgs_ext } from 'wasp/src/rpcTests/operations/jsDefinitions'

// PRIVATE API
export type CustomSignup_ext = typeof signup_ext

// PUBLIC API
export const customSignup: AuthenticatedOperationFor<CustomSignup_ext> =
  createAuthenticatedOperation(
    signup_ext,
    {
    },
  )

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
export type UpdateTaskIsDone_ext = typeof updateTaskIsDone_ext

// PUBLIC API
export const updateTaskIsDone: AuthenticatedOperationFor<UpdateTaskIsDone_ext> =
  createAuthenticatedOperation(
    updateTaskIsDone_ext,
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
export type ToggleAllTasks_ext = typeof toggleAllTasks_ext

// PUBLIC API
export const toggleAllTasks: AuthenticatedOperationFor<ToggleAllTasks_ext> =
  createAuthenticatedOperation(
    toggleAllTasks_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type RequestUppercaseText_ext = typeof requestUppercaseText_ext

// PUBLIC API
export const requestUppercaseText: AuthenticatedOperationFor<RequestUppercaseText_ext> =
  createAuthenticatedOperation(
    requestUppercaseText_ext,
    {
      UppercaseTextRequest: prisma.uppercaseTextRequest,
    },
  )

// PRIVATE API
export type TestingAction_ext = typeof testingAction_ext

// PUBLIC API
export const testingAction: AuthenticatedOperationFor<TestingAction_ext> =
  createAuthenticatedOperation(
    testingAction_ext,
    {
    },
  )

// PRIVATE API
export type TaskToTaskUnspecified_ext = typeof taskToTaskUnspecified_ext

// PUBLIC API
export const taskToTaskUnspecified: AuthenticatedOperationFor<TaskToTaskUnspecified_ext> =
  createAuthenticatedOperation(
    taskToTaskUnspecified_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type TaskToTaskSatisfies_ext = typeof taskToTaskSatisfies_ext

// PUBLIC API
export const taskToTaskSatisfies: AuthenticatedOperationFor<TaskToTaskSatisfies_ext> =
  createAuthenticatedOperation(
    taskToTaskSatisfies_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type TaskToTaskSpecified_ext = typeof taskToTaskSpecified_ext

// PUBLIC API
export const taskToTaskSpecified: AuthenticatedOperationFor<TaskToTaskSpecified_ext> =
  createAuthenticatedOperation(
    taskToTaskSpecified_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type VoidToStringAuth_ext = typeof voidToStringAuth_ext

// PUBLIC API
export const voidToStringAuth: AuthenticatedOperationFor<VoidToStringAuth_ext> =
  createAuthenticatedOperation(
    voidToStringAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type VoidToStringNoAuth_ext = typeof voidToStringNoAuth_ext

// PUBLIC API
export const voidToStringNoAuth: UnauthenticatedOperationFor<VoidToStringNoAuth_ext> =
  createUnauthenticatedOperation(
    voidToStringNoAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type UnspecifiedToNumber_ext = typeof unspecifiedToNumber_ext

// PUBLIC API
export const unspecifiedToNumber: AuthenticatedOperationFor<UnspecifiedToNumber_ext> =
  createAuthenticatedOperation(
    unspecifiedToNumber_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type BoolToStringAuth_ext = typeof boolToStringAuth_ext

// PUBLIC API
export const boolToStringAuth: AuthenticatedOperationFor<BoolToStringAuth_ext> =
  createAuthenticatedOperation(
    boolToStringAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type BoolToStringNoAuth_ext = typeof boolToStringNoAuth_ext

// PUBLIC API
export const boolToStringNoAuth: UnauthenticatedOperationFor<BoolToStringNoAuth_ext> =
  createUnauthenticatedOperation(
    boolToStringNoAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type BoolToVoidNoAuth_ext = typeof boolToVoidNoAuth_ext

// PUBLIC API
export const boolToVoidNoAuth: UnauthenticatedOperationFor<BoolToVoidNoAuth_ext> =
  createUnauthenticatedOperation(
    boolToVoidNoAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type BoolToVoidAuth_ext = typeof boolToVoidAuth_ext

// PUBLIC API
export const boolToVoidAuth: AuthenticatedOperationFor<BoolToVoidAuth_ext> =
  createAuthenticatedOperation(
    boolToVoidAuth_ext,
    {
      Task: prisma.task,
    },
  )

// PRIVATE API
export type JsActionWithArgs_ext = typeof jsActionWithArgs_ext

// PUBLIC API
export const jsActionWithArgs: AuthenticatedOperationFor<JsActionWithArgs_ext> =
  createAuthenticatedOperation(
    jsActionWithArgs_ext,
    {
      Task: prisma.task,
    },
  )
