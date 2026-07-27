
declare module "virtual:wasp/user/env" {
  import type { RegisteredClientEnvValidationSchema } from "./client/env/schema";

  export const clientEnvValidationSchema: RegisteredClientEnvValidationSchema;
}

declare module "virtual:wasp/user/env" {
  import type { RegisteredServerEnvValidationSchema } from "./server/env";

  export const serverEnvValidationSchema: RegisteredServerEnvValidationSchema;
}

declare module "virtual:wasp/user/features/db/prisma" {
  import type { RegisteredPrismaSetupFn } from "./server/dbClient"

  export const setUpPrisma: RegisteredPrismaSetupFn;
}

declare module "virtual:wasp/user/features/auth/customSignup" {
  import { RegisteredCustomSignup } from "./server/operations/actions/index";

  export const customSignup: RegisteredCustomSignup;
}

declare module "virtual:wasp/user/features/operations/actions" {
  import { RegisteredCreateTask } from "./server/operations/actions/index";

  export const createTask: RegisteredCreateTask;
}

declare module "virtual:wasp/user/features/operations/actions" {
  import { RegisteredUpdateTaskIsDone } from "./server/operations/actions/index";

  export const updateTaskIsDone: RegisteredUpdateTaskIsDone;
}

declare module "virtual:wasp/user/features/operations/actions" {
  import { RegisteredDeleteCompletedTasks } from "./server/operations/actions/index";

  export const deleteCompletedTasks: RegisteredDeleteCompletedTasks;
}

declare module "virtual:wasp/user/features/operations/actions" {
  import { RegisteredToggleAllTasks } from "./server/operations/actions/index";

  export const toggleAllTasks: RegisteredToggleAllTasks;
}

declare module "virtual:wasp/user/features/jobs/uppercaseText" {
  import { RegisteredRequestUppercaseText } from "./server/operations/actions/index";

  export const requestUppercaseText: RegisteredRequestUppercaseText;
}

declare module "virtual:wasp/user/rpcTests/operations/server" {
  import { RegisteredTestingAction } from "./server/operations/actions/index";

  export const testingAction: RegisteredTestingAction;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredTaskToTaskUnspecified } from "./server/operations/actions/index";

  export const taskToTaskUnspecified: RegisteredTaskToTaskUnspecified;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredTaskToTaskSatisfies } from "./server/operations/actions/index";

  export const taskToTaskSatisfies: RegisteredTaskToTaskSatisfies;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredTaskToTaskSpecified } from "./server/operations/actions/index";

  export const taskToTaskSpecified: RegisteredTaskToTaskSpecified;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredVoidToStringAuth } from "./server/operations/actions/index";

  export const voidToStringAuth: RegisteredVoidToStringAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredVoidToStringNoAuth } from "./server/operations/actions/index";

  export const voidToStringNoAuth: RegisteredVoidToStringNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredUnspecifiedToNumber } from "./server/operations/actions/index";

  export const unspecifiedToNumber: RegisteredUnspecifiedToNumber;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredBoolToStringAuth } from "./server/operations/actions/index";

  export const boolToStringAuth: RegisteredBoolToStringAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredBoolToStringNoAuth } from "./server/operations/actions/index";

  export const boolToStringNoAuth: RegisteredBoolToStringNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredBoolToVoidNoAuth } from "./server/operations/actions/index";

  export const boolToVoidNoAuth: RegisteredBoolToVoidNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredBoolToVoidAuth } from "./server/operations/actions/index";

  export const boolToVoidAuth: RegisteredBoolToVoidAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/jsDefinitions" {
  import { RegisteredJsActionWithArgs } from "./server/operations/actions/index";

  export const jsActionWithArgs: RegisteredJsActionWithArgs;
}

declare module "virtual:wasp/user/features/operations/queries" {
  import { RegisteredGetTasks } from "./server/operations/queries/index";

  export const getTasks: RegisteredGetTasks;
}

declare module "virtual:wasp/user/features/operations/queries" {
  import { RegisteredGetNumTasks } from "./server/operations/queries/index";

  export const getNumTasks: RegisteredGetNumTasks;
}

declare module "virtual:wasp/user/features/operations/queries" {
  import { RegisteredGetTask } from "./server/operations/queries/index";

  export const getTask: RegisteredGetTask;
}

declare module "virtual:wasp/user/features/operations/queries" {
  import { RegisteredGetSerializedObjects } from "./server/operations/queries/index";

  export const getSerializedObjects: RegisteredGetSerializedObjects;
}

declare module "virtual:wasp/user/features/jobs/uppercaseText" {
  import { RegisteredGetTextUppercaseRequests } from "./server/operations/queries/index";

  export const getTextUppercaseRequests: RegisteredGetTextUppercaseRequests;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredGetDate } from "./server/operations/queries/index";

  export const getDate: RegisteredGetDate;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredGetAnythingNoAuth } from "./server/operations/queries/index";

  export const getAnythingNoAuth: RegisteredGetAnythingNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredGetAnythingAuth } from "./server/operations/queries/index";

  export const getAnythingAuth: RegisteredGetAnythingAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredGetTrueVoid } from "./server/operations/queries/index";

  export const getTrueVoid: RegisteredGetTrueVoid;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredGetAnyNoAuth } from "./server/operations/queries/index";

  export const getAnyNoAuth: RegisteredGetAnyNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredGetAnyAuth } from "./server/operations/queries/index";

  export const getAnyAuth: RegisteredGetAnyAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  import { RegisteredGetAnyToNumberSpecified } from "./server/operations/queries/index";

  export const getAnyToNumberSpecified: RegisteredGetAnyToNumberSpecified;
}
