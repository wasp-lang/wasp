
import { prisma } from '../../index'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers'
import type { OperationFromRegister } from '../register'
import type {
  GetTasks,
  GetNumTasks,
  GetTask,
  GetSerializedObjects,
  GetTextUppercaseRequests,
  GetDate,
  GetAnythingNoAuth,
  GetAnythingAuth,
  GetTrueVoid,
  GetAnyNoAuth,
  GetAnyAuth,
  GetAnyToNumberSpecified,
} from './types'
import { getTasks as getTasks_ext } from 'virtual:wasp/user/features/operations/queries'
import { getNumTasks as getNumTasks_ext } from 'virtual:wasp/user/features/operations/queries'
import { getTask as getTask_ext } from 'virtual:wasp/user/features/operations/queries'
import { getSerializedObjects as getSerializedObjects_ext } from 'virtual:wasp/user/features/operations/queries'
import { getTextUppercaseRequests as getTextUppercaseRequests_ext } from 'virtual:wasp/user/features/jobs/uppercaseText'
import { getDate as getDate_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { getAnythingNoAuth as getAnythingNoAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { getAnythingAuth as getAnythingAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { getTrueVoid as getTrueVoid_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { getAnyNoAuth as getAnyNoAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { getAnyAuth as getAnyAuth_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'
import { getAnyToNumberSpecified as getAnyToNumberSpecified_ext } from 'virtual:wasp/user/rpcTests/operations/definitions'

// PRIVATE API
export type RegisteredGetTasks = OperationFromRegister<'getTasks', GetTasks>

// PUBLIC API
export const getTasks: AuthenticatedOperationFor<RegisteredGetTasks> =
  createAuthenticatedOperation<RegisteredGetTasks>(
    () => getTasks_ext,
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type RegisteredGetNumTasks = OperationFromRegister<'getNumTasks', GetNumTasks>

// PUBLIC API
export const getNumTasks: UnauthenticatedOperationFor<RegisteredGetNumTasks> =
  createUnauthenticatedOperation<RegisteredGetNumTasks>(
    () => getNumTasks_ext,
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type RegisteredGetTask = OperationFromRegister<'getTask', GetTask>

// PUBLIC API
export const getTask: AuthenticatedOperationFor<RegisteredGetTask> =
  createAuthenticatedOperation<RegisteredGetTask>(
    () => getTask_ext,
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type RegisteredGetSerializedObjects = OperationFromRegister<'getSerializedObjects', GetSerializedObjects>

// PUBLIC API
export const getSerializedObjects: AuthenticatedOperationFor<RegisteredGetSerializedObjects> =
  createAuthenticatedOperation<RegisteredGetSerializedObjects>(
    () => getSerializedObjects_ext,
    {
    },
  )


// PRIVATE API
export type RegisteredGetTextUppercaseRequests = OperationFromRegister<'getTextUppercaseRequests', GetTextUppercaseRequests>

// PUBLIC API
export const getTextUppercaseRequests: AuthenticatedOperationFor<RegisteredGetTextUppercaseRequests> =
  createAuthenticatedOperation<RegisteredGetTextUppercaseRequests>(
    () => getTextUppercaseRequests_ext,
    {
      UppercaseTextRequest: prisma.uppercaseTextRequest,
    },
  )


// PRIVATE API
export type RegisteredGetDate = OperationFromRegister<'getDate', GetDate>

// PUBLIC API
export const getDate: AuthenticatedOperationFor<RegisteredGetDate> =
  createAuthenticatedOperation<RegisteredGetDate>(
    () => getDate_ext,
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnythingNoAuth = OperationFromRegister<'getAnythingNoAuth', GetAnythingNoAuth>

// PUBLIC API
export const getAnythingNoAuth: UnauthenticatedOperationFor<RegisteredGetAnythingNoAuth> =
  createUnauthenticatedOperation<RegisteredGetAnythingNoAuth>(
    () => getAnythingNoAuth_ext,
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnythingAuth = OperationFromRegister<'getAnythingAuth', GetAnythingAuth>

// PUBLIC API
export const getAnythingAuth: AuthenticatedOperationFor<RegisteredGetAnythingAuth> =
  createAuthenticatedOperation<RegisteredGetAnythingAuth>(
    () => getAnythingAuth_ext,
    {
    },
  )


// PRIVATE API
export type RegisteredGetTrueVoid = OperationFromRegister<'getTrueVoid', GetTrueVoid>

// PUBLIC API
export const getTrueVoid: AuthenticatedOperationFor<RegisteredGetTrueVoid> =
  createAuthenticatedOperation<RegisteredGetTrueVoid>(
    () => getTrueVoid_ext,
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnyNoAuth = OperationFromRegister<'getAnyNoAuth', GetAnyNoAuth>

// PUBLIC API
export const getAnyNoAuth: UnauthenticatedOperationFor<RegisteredGetAnyNoAuth> =
  createUnauthenticatedOperation<RegisteredGetAnyNoAuth>(
    () => getAnyNoAuth_ext,
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnyAuth = OperationFromRegister<'getAnyAuth', GetAnyAuth>

// PUBLIC API
export const getAnyAuth: AuthenticatedOperationFor<RegisteredGetAnyAuth> =
  createAuthenticatedOperation<RegisteredGetAnyAuth>(
    () => getAnyAuth_ext,
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnyToNumberSpecified = OperationFromRegister<'getAnyToNumberSpecified', GetAnyToNumberSpecified>

// PUBLIC API
export const getAnyToNumberSpecified: AuthenticatedOperationFor<RegisteredGetAnyToNumberSpecified> =
  createAuthenticatedOperation<RegisteredGetAnyToNumberSpecified>(
    () => getAnyToNumberSpecified_ext,
    {
    },
  )

