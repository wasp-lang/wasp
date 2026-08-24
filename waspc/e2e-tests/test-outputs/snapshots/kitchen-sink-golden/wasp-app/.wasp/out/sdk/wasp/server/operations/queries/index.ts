
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
  GetTasks,
  GetNumTasks,
  GetTask,
  GetOldestTask,
  GetSerializedObjects,
  GetTextUppercaseRequests,
  GetDate,
  GetAnythingNoAuth,
  GetAnythingAuth,
  GetTrueVoid,
  GetAnyNoAuth,
  GetAnyAuth,
  GetAnyToNumberSpecified,
} from './types.js'

// PRIVATE API
export type RegisteredGetTasks = FromRegisterPath<['operations', 'getTasks'], GetTasks>

// PUBLIC API
export const getTasks: AuthenticatedOperationFor<RegisteredGetTasks> =
  createAuthenticatedOperation<RegisteredGetTasks>(
    () => getServerOperation<RegisteredGetTasks>('getTasks'),
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type RegisteredGetNumTasks = FromRegisterPath<['operations', 'getNumTasks'], GetNumTasks>

// PUBLIC API
export const getNumTasks: UnauthenticatedOperationFor<RegisteredGetNumTasks> =
  createUnauthenticatedOperation<RegisteredGetNumTasks>(
    () => getServerOperation<RegisteredGetNumTasks>('getNumTasks'),
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type RegisteredGetTask = FromRegisterPath<['operations', 'getTask'], GetTask>

// PUBLIC API
export const getTask: AuthenticatedOperationFor<RegisteredGetTask> =
  createAuthenticatedOperation<RegisteredGetTask>(
    () => getServerOperation<RegisteredGetTask>('getTask'),
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type RegisteredGetOldestTask = FromRegisterPath<['operations', 'getOldestTask'], GetOldestTask>

// PUBLIC API
export const getOldestTask: AuthenticatedOperationFor<RegisteredGetOldestTask> =
  createAuthenticatedOperation<RegisteredGetOldestTask>(
    () => getServerOperation<RegisteredGetOldestTask>('getOldestTask'),
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type RegisteredGetSerializedObjects = FromRegisterPath<['operations', 'getSerializedObjects'], GetSerializedObjects>

// PUBLIC API
export const getSerializedObjects: AuthenticatedOperationFor<RegisteredGetSerializedObjects> =
  createAuthenticatedOperation<RegisteredGetSerializedObjects>(
    () => getServerOperation<RegisteredGetSerializedObjects>('getSerializedObjects'),
    {
    },
  )


// PRIVATE API
export type RegisteredGetTextUppercaseRequests = FromRegisterPath<['operations', 'getTextUppercaseRequests'], GetTextUppercaseRequests>

// PUBLIC API
export const getTextUppercaseRequests: AuthenticatedOperationFor<RegisteredGetTextUppercaseRequests> =
  createAuthenticatedOperation<RegisteredGetTextUppercaseRequests>(
    () => getServerOperation<RegisteredGetTextUppercaseRequests>('getTextUppercaseRequests'),
    {
      UppercaseTextRequest: prisma.uppercaseTextRequest,
    },
  )


// PRIVATE API
export type RegisteredGetDate = FromRegisterPath<['operations', 'getDate'], GetDate>

// PUBLIC API
export const getDate: AuthenticatedOperationFor<RegisteredGetDate> =
  createAuthenticatedOperation<RegisteredGetDate>(
    () => getServerOperation<RegisteredGetDate>('getDate'),
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnythingNoAuth = FromRegisterPath<['operations', 'getAnythingNoAuth'], GetAnythingNoAuth>

// PUBLIC API
export const getAnythingNoAuth: UnauthenticatedOperationFor<RegisteredGetAnythingNoAuth> =
  createUnauthenticatedOperation<RegisteredGetAnythingNoAuth>(
    () => getServerOperation<RegisteredGetAnythingNoAuth>('getAnythingNoAuth'),
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnythingAuth = FromRegisterPath<['operations', 'getAnythingAuth'], GetAnythingAuth>

// PUBLIC API
export const getAnythingAuth: AuthenticatedOperationFor<RegisteredGetAnythingAuth> =
  createAuthenticatedOperation<RegisteredGetAnythingAuth>(
    () => getServerOperation<RegisteredGetAnythingAuth>('getAnythingAuth'),
    {
    },
  )


// PRIVATE API
export type RegisteredGetTrueVoid = FromRegisterPath<['operations', 'getTrueVoid'], GetTrueVoid>

// PUBLIC API
export const getTrueVoid: AuthenticatedOperationFor<RegisteredGetTrueVoid> =
  createAuthenticatedOperation<RegisteredGetTrueVoid>(
    () => getServerOperation<RegisteredGetTrueVoid>('getTrueVoid'),
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnyNoAuth = FromRegisterPath<['operations', 'getAnyNoAuth'], GetAnyNoAuth>

// PUBLIC API
export const getAnyNoAuth: UnauthenticatedOperationFor<RegisteredGetAnyNoAuth> =
  createUnauthenticatedOperation<RegisteredGetAnyNoAuth>(
    () => getServerOperation<RegisteredGetAnyNoAuth>('getAnyNoAuth'),
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnyAuth = FromRegisterPath<['operations', 'getAnyAuth'], GetAnyAuth>

// PUBLIC API
export const getAnyAuth: AuthenticatedOperationFor<RegisteredGetAnyAuth> =
  createAuthenticatedOperation<RegisteredGetAnyAuth>(
    () => getServerOperation<RegisteredGetAnyAuth>('getAnyAuth'),
    {
    },
  )


// PRIVATE API
export type RegisteredGetAnyToNumberSpecified = FromRegisterPath<['operations', 'getAnyToNumberSpecified'], GetAnyToNumberSpecified>

// PUBLIC API
export const getAnyToNumberSpecified: AuthenticatedOperationFor<RegisteredGetAnyToNumberSpecified> =
  createAuthenticatedOperation<RegisteredGetAnyToNumberSpecified>(
    () => getServerOperation<RegisteredGetAnyToNumberSpecified>('getAnyToNumberSpecified'),
    {
    },
  )

