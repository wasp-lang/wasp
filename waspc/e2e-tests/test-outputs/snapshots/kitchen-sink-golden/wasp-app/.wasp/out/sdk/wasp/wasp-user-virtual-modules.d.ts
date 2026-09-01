/**
 * Declares the virtual user modules the SDK imports.
 * 
 * The types are written as inline `import("...")` types on purpose.
 * Ambient module declarations can't reach another module through a
 * relative import statement (TS2439).
 */

declare module "virtual:wasp/user/env" {
  export const clientEnvValidationSchema: import("./client/env/schema").RegisteredClientEnvValidationSchema;
}

declare module "virtual:wasp/user/env" {
  export const serverEnvValidationSchema: import("./server/env").RegisteredServerEnvValidationSchema;
}

declare module "virtual:wasp/user/features/db/prisma" {
  export const setUpPrisma: import("./server/dbClient").RegisteredPrismaSetupFn;
}

declare module "virtual:wasp/user/features/auth/hooks" {
  export const onBeforeSignup: import("./server/auth/hooks").OnBeforeSignupHook;
}

declare module "virtual:wasp/user/features/auth/hooks" {
  export const onAfterSignup: import("./server/auth/hooks").OnAfterSignupHook;
}

declare module "virtual:wasp/user/features/auth/hooks" {
  export const onBeforeLogin: import("./server/auth/hooks").OnBeforeLoginHook;
}

declare module "virtual:wasp/user/features/auth/hooks" {
  export const onAfterLogin: import("./server/auth/hooks").OnAfterLoginHook;
}

declare module "virtual:wasp/user/features/auth/providers/email" {
  export const emailUserSignupFields: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/google" {
  export const googleUserSignupFields: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/github" {
  export const gitHubUserSignupFields: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/slack" {
  export const slackUserSignupFields: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/discord" {
  export const discordUserSignupFields: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/microsoft" {
  export const microsoftUserSignupFields: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/google" {
  export const googleConfig: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/github" {
  export const gitHubConfig: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/slack" {
  export const slackConfig: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/discord" {
  export const discordConfig: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/microsoft" {
  export const microsoftConfig: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/email" {
  export const getVerificationEmailContent: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/providers/email" {
  export const getPasswordResetEmailContent: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/auth/hooks" {
  export const onAfterEmailVerified: import("./server/auth/provider/types").WaspAuthExtension;
}

declare module "virtual:wasp/user/features/operations/queries" {
  export const getTasks: import("./server/operations/queries/index").RegisteredGetTasks;
}

declare module "virtual:wasp/user/features/operations/queries" {
  export const getNumTasks: import("./server/operations/queries/index").RegisteredGetNumTasks;
}

declare module "virtual:wasp/user/features/operations/queries" {
  export const getTask: import("./server/operations/queries/index").RegisteredGetTask;
}

declare module "virtual:wasp/user/features/operations/getOldestTask" {
  const _default: import("./server/operations/queries/index").RegisteredGetOldestTask;
  export default _default;
}

declare module "virtual:wasp/user/features/operations/queries" {
  export const getSerializedObjects: import("./server/operations/queries/index").RegisteredGetSerializedObjects;
}

declare module "virtual:wasp/user/features/jobs/uppercaseText" {
  export const getTextUppercaseRequests: import("./server/operations/queries/index").RegisteredGetTextUppercaseRequests;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const getDate: import("./server/operations/queries/index").RegisteredGetDate;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const getAnythingNoAuth: import("./server/operations/queries/index").RegisteredGetAnythingNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const getAnythingAuth: import("./server/operations/queries/index").RegisteredGetAnythingAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const getTrueVoid: import("./server/operations/queries/index").RegisteredGetTrueVoid;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const getAnyNoAuth: import("./server/operations/queries/index").RegisteredGetAnyNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const getAnyAuth: import("./server/operations/queries/index").RegisteredGetAnyAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const getAnyToNumberSpecified: import("./server/operations/queries/index").RegisteredGetAnyToNumberSpecified;
}

declare module "virtual:wasp/user/features/auth/customSignup" {
  export const customSignup: import("./server/operations/actions/index").RegisteredCustomSignup;
}

declare module "virtual:wasp/user/features/operations/actions" {
  export const createTask: import("./server/operations/actions/index").RegisteredCreateTask;
}

declare module "virtual:wasp/user/features/operations/actions" {
  export const updateTaskIsDone: import("./server/operations/actions/index").RegisteredUpdateTaskIsDone;
}

declare module "virtual:wasp/user/features/operations/actions" {
  export const deleteCompletedTasks: import("./server/operations/actions/index").RegisteredDeleteCompletedTasks;
}

declare module "virtual:wasp/user/features/operations/actions" {
  export const toggleAllTasks: import("./server/operations/actions/index").RegisteredToggleAllTasks;
}

declare module "virtual:wasp/user/features/jobs/uppercaseText" {
  export const requestUppercaseText: import("./server/operations/actions/index").RegisteredRequestUppercaseText;
}

declare module "virtual:wasp/user/rpcTests/operations/server" {
  export const testingAction: import("./server/operations/actions/index").RegisteredTestingAction;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const taskToTaskUnspecified: import("./server/operations/actions/index").RegisteredTaskToTaskUnspecified;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const taskToTaskSatisfies: import("./server/operations/actions/index").RegisteredTaskToTaskSatisfies;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const taskToTaskSpecified: import("./server/operations/actions/index").RegisteredTaskToTaskSpecified;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const voidToStringAuth: import("./server/operations/actions/index").RegisteredVoidToStringAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const voidToStringNoAuth: import("./server/operations/actions/index").RegisteredVoidToStringNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const unspecifiedToNumber: import("./server/operations/actions/index").RegisteredUnspecifiedToNumber;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const boolToStringAuth: import("./server/operations/actions/index").RegisteredBoolToStringAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const boolToStringNoAuth: import("./server/operations/actions/index").RegisteredBoolToStringNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const boolToVoidNoAuth: import("./server/operations/actions/index").RegisteredBoolToVoidNoAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/definitions" {
  export const boolToVoidAuth: import("./server/operations/actions/index").RegisteredBoolToVoidAuth;
}

declare module "virtual:wasp/user/rpcTests/operations/jsDefinitions" {
  export const jsActionWithArgs: import("./server/operations/actions/index").RegisteredJsActionWithArgs;
}
