
import { prisma } from 'wasp/server'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
  type AuthenticatedOperationFor,
  createAuthenticatedOperation,
} from '../wrappers.js'
import { getTasks as getTasks_ext } from 'wasp/src/features/operations/queries'
import { getNumTasks as getNumTasks_ext } from 'wasp/src/features/operations/queries'
import { getTask as getTask_ext } from 'wasp/src/features/operations/queries'
import { getSerializedObjects as getSerializedObjects_ext } from 'wasp/src/features/operations/queries'
import { getTextUppercaseRequests as getTextUppercaseRequests_ext } from 'wasp/src/features/jobs/uppercaseText'
import { getDate as getDate_ext } from 'wasp/src/rpcTests/operations/definitions'
import { getAnythingNoAuth as getAnythingNoAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { getAnythingAuth as getAnythingAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { getTrueVoid as getTrueVoid_ext } from 'wasp/src/rpcTests/operations/definitions'
import { getAnyNoAuth as getAnyNoAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { getAnyAuth as getAnyAuth_ext } from 'wasp/src/rpcTests/operations/definitions'
import { getAnyToNumberSpecified as getAnyToNumberSpecified_ext } from 'wasp/src/rpcTests/operations/definitions'

// PRIVATE API
export type GetTasks_ext = typeof getTasks_ext

// PUBLIC API
export const getTasks: AuthenticatedOperationFor<GetTasks_ext> =
  createAuthenticatedOperation(
    getTasks_ext,
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type GetNumTasks_ext = typeof getNumTasks_ext

// PUBLIC API
export const getNumTasks: UnauthenticatedOperationFor<GetNumTasks_ext> =
  createUnauthenticatedOperation(
    getNumTasks_ext,
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type GetTask_ext = typeof getTask_ext

// PUBLIC API
export const getTask: AuthenticatedOperationFor<GetTask_ext> =
  createAuthenticatedOperation(
    getTask_ext,
    {
      Task: prisma.task,
    },
  )


// PRIVATE API
export type GetSerializedObjects_ext = typeof getSerializedObjects_ext

// PUBLIC API
export const getSerializedObjects: AuthenticatedOperationFor<GetSerializedObjects_ext> =
  createAuthenticatedOperation(
    getSerializedObjects_ext,
    {
    },
  )


// PRIVATE API
export type GetTextUppercaseRequests_ext = typeof getTextUppercaseRequests_ext

// PUBLIC API
export const getTextUppercaseRequests: AuthenticatedOperationFor<GetTextUppercaseRequests_ext> =
  createAuthenticatedOperation(
    getTextUppercaseRequests_ext,
    {
      UppercaseTextRequest: prisma.uppercaseTextRequest,
    },
  )


// PRIVATE API
export type GetDate_ext = typeof getDate_ext

// PUBLIC API
export const getDate: AuthenticatedOperationFor<GetDate_ext> =
  createAuthenticatedOperation(
    getDate_ext,
    {
    },
  )


// PRIVATE API
export type GetAnythingNoAuth_ext = typeof getAnythingNoAuth_ext

// PUBLIC API
export const getAnythingNoAuth: UnauthenticatedOperationFor<GetAnythingNoAuth_ext> =
  createUnauthenticatedOperation(
    getAnythingNoAuth_ext,
    {
    },
  )


// PRIVATE API
export type GetAnythingAuth_ext = typeof getAnythingAuth_ext

// PUBLIC API
export const getAnythingAuth: AuthenticatedOperationFor<GetAnythingAuth_ext> =
  createAuthenticatedOperation(
    getAnythingAuth_ext,
    {
    },
  )


// PRIVATE API
export type GetTrueVoid_ext = typeof getTrueVoid_ext

// PUBLIC API
export const getTrueVoid: AuthenticatedOperationFor<GetTrueVoid_ext> =
  createAuthenticatedOperation(
    getTrueVoid_ext,
    {
    },
  )


// PRIVATE API
export type GetAnyNoAuth_ext = typeof getAnyNoAuth_ext

// PUBLIC API
export const getAnyNoAuth: UnauthenticatedOperationFor<GetAnyNoAuth_ext> =
  createUnauthenticatedOperation(
    getAnyNoAuth_ext,
    {
    },
  )


// PRIVATE API
export type GetAnyAuth_ext = typeof getAnyAuth_ext

// PUBLIC API
export const getAnyAuth: AuthenticatedOperationFor<GetAnyAuth_ext> =
  createAuthenticatedOperation(
    getAnyAuth_ext,
    {
    },
  )


// PRIVATE API
export type GetAnyToNumberSpecified_ext = typeof getAnyToNumberSpecified_ext

// PUBLIC API
export const getAnyToNumberSpecified: AuthenticatedOperationFor<GetAnyToNumberSpecified_ext> =
  createAuthenticatedOperation(
    getAnyToNumberSpecified_ext,
    {
    },
  )

