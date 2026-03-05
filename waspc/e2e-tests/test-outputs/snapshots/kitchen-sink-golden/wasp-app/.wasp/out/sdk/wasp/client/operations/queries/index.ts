import { type QueryFor, createQuery } from './core'
import { GetTasks_ext } from 'wasp/server/operations/queries'
import { GetNumTasks_ext } from 'wasp/server/operations/queries'
import { GetTask_ext } from 'wasp/server/operations/queries'
import { GetSerializedObjects_ext } from 'wasp/server/operations/queries'
import { GetTextUppercaseRequests_ext } from 'wasp/server/operations/queries'
import { GetDate_ext } from 'wasp/server/operations/queries'
import { GetAnythingNoAuth_ext } from 'wasp/server/operations/queries'
import { GetAnythingAuth_ext } from 'wasp/server/operations/queries'
import { GetTrueVoid_ext } from 'wasp/server/operations/queries'
import { GetAnyNoAuth_ext } from 'wasp/server/operations/queries'
import { GetAnyAuth_ext } from 'wasp/server/operations/queries'
import { GetAnyToNumberSpecified_ext } from 'wasp/server/operations/queries'

// PUBLIC API
export const getTasks: QueryFor<GetTasks_ext> = createQuery<GetTasks_ext>(
  'operations/get-tasks',
  ['Task'],
)

// PUBLIC API
export const getNumTasks: QueryFor<GetNumTasks_ext> = createQuery<GetNumTasks_ext>(
  'operations/get-num-tasks',
  ['Task'],
)

// PUBLIC API
export const getTask: QueryFor<GetTask_ext> = createQuery<GetTask_ext>(
  'operations/get-task',
  ['Task'],
)

// PUBLIC API
export const getSerializedObjects: QueryFor<GetSerializedObjects_ext> = createQuery<GetSerializedObjects_ext>(
  'operations/get-serialized-objects',
  [],
)

// PUBLIC API
export const getTextUppercaseRequests: QueryFor<GetTextUppercaseRequests_ext> = createQuery<GetTextUppercaseRequests_ext>(
  'operations/get-text-uppercase-requests',
  ['UppercaseTextRequest'],
)

// PUBLIC API
export const getDate: QueryFor<GetDate_ext> = createQuery<GetDate_ext>(
  'operations/get-date',
  [],
)

// PUBLIC API
export const getAnythingNoAuth: QueryFor<GetAnythingNoAuth_ext> = createQuery<GetAnythingNoAuth_ext>(
  'operations/get-anything-no-auth',
  [],
)

// PUBLIC API
export const getAnythingAuth: QueryFor<GetAnythingAuth_ext> = createQuery<GetAnythingAuth_ext>(
  'operations/get-anything-auth',
  [],
)

// PUBLIC API
export const getTrueVoid: QueryFor<GetTrueVoid_ext> = createQuery<GetTrueVoid_ext>(
  'operations/get-true-void',
  [],
)

// PUBLIC API
export const getAnyNoAuth: QueryFor<GetAnyNoAuth_ext> = createQuery<GetAnyNoAuth_ext>(
  'operations/get-any-no-auth',
  [],
)

// PUBLIC API
export const getAnyAuth: QueryFor<GetAnyAuth_ext> = createQuery<GetAnyAuth_ext>(
  'operations/get-any-auth',
  [],
)

// PUBLIC API
export const getAnyToNumberSpecified: QueryFor<GetAnyToNumberSpecified_ext> = createQuery<GetAnyToNumberSpecified_ext>(
  'operations/get-any-to-number-specified',
  [],
)

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from './core'
