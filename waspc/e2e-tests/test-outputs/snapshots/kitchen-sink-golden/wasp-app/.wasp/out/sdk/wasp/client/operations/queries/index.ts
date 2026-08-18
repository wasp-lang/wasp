import { type QueryFor, createQuery } from "./core";
import type {
  RegisteredGetTasks,
  RegisteredGetNumTasks,
  RegisteredGetTask,
  RegisteredGetOldestTask,
  RegisteredGetSerializedObjects,
  RegisteredGetTextUppercaseRequests,
  RegisteredGetDate,
  RegisteredGetAnythingNoAuth,
  RegisteredGetAnythingAuth,
  RegisteredGetTrueVoid,
  RegisteredGetAnyNoAuth,
  RegisteredGetAnyAuth,
  RegisteredGetAnyToNumberSpecified,
} from "../../../server/operations/queries/index";

// PUBLIC API
export const getTasks: QueryFor<RegisteredGetTasks> = createQuery<RegisteredGetTasks>(
  "operations/get-tasks",
  ['Task'],
)

// PUBLIC API
export const getNumTasks: QueryFor<RegisteredGetNumTasks> = createQuery<RegisteredGetNumTasks>(
  "operations/get-num-tasks",
  ['Task'],
)

// PUBLIC API
export const getTask: QueryFor<RegisteredGetTask> = createQuery<RegisteredGetTask>(
  "operations/get-task",
  ['Task'],
)

// PUBLIC API
export const getOldestTask: QueryFor<RegisteredGetOldestTask> = createQuery<RegisteredGetOldestTask>(
  "operations/get-oldest-task",
  ['Task'],
)

// PUBLIC API
export const getSerializedObjects: QueryFor<RegisteredGetSerializedObjects> = createQuery<RegisteredGetSerializedObjects>(
  "operations/get-serialized-objects",
  [],
)

// PUBLIC API
export const getTextUppercaseRequests: QueryFor<RegisteredGetTextUppercaseRequests> = createQuery<RegisteredGetTextUppercaseRequests>(
  "operations/get-text-uppercase-requests",
  ['UppercaseTextRequest'],
)

// PUBLIC API
export const getDate: QueryFor<RegisteredGetDate> = createQuery<RegisteredGetDate>(
  "operations/get-date",
  [],
)

// PUBLIC API
export const getAnythingNoAuth: QueryFor<RegisteredGetAnythingNoAuth> = createQuery<RegisteredGetAnythingNoAuth>(
  "operations/get-anything-no-auth",
  [],
)

// PUBLIC API
export const getAnythingAuth: QueryFor<RegisteredGetAnythingAuth> = createQuery<RegisteredGetAnythingAuth>(
  "operations/get-anything-auth",
  [],
)

// PUBLIC API
export const getTrueVoid: QueryFor<RegisteredGetTrueVoid> = createQuery<RegisteredGetTrueVoid>(
  "operations/get-true-void",
  [],
)

// PUBLIC API
export const getAnyNoAuth: QueryFor<RegisteredGetAnyNoAuth> = createQuery<RegisteredGetAnyNoAuth>(
  "operations/get-any-no-auth",
  [],
)

// PUBLIC API
export const getAnyAuth: QueryFor<RegisteredGetAnyAuth> = createQuery<RegisteredGetAnyAuth>(
  "operations/get-any-auth",
  [],
)

// PUBLIC API
export const getAnyToNumberSpecified: QueryFor<RegisteredGetAnyToNumberSpecified> = createQuery<RegisteredGetAnyToNumberSpecified>(
  "operations/get-any-to-number-specified",
  [],
)

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from "./core"
