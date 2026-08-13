/**
 * Declares the virtual user modules the SDK imports.
 * 
 * The types are written as inline `import("...")` types on purpose.
 * Ambient module declarations can't reach another module through a
 * relative import statement (TS2439).
 */

declare module "virtual:wasp/user/env" {
  export const clientEnvSchema: import("./client/env/schema").RegisteredClientEnvValidationSchema;
}

declare module "virtual:wasp/user/auth/provider" {
  export const clerkAuthProvider: import("./server/auth/provider/types").RegisteredAuthProvider;
}

declare module "virtual:wasp/user/operations" {
  export const getMyTasks: import("./server/operations/queries/index").RegisteredGetMyTasks;
}

declare module "virtual:wasp/user/operations" {
  export const createTask: import("./server/operations/actions/index").RegisteredCreateTask;
}
