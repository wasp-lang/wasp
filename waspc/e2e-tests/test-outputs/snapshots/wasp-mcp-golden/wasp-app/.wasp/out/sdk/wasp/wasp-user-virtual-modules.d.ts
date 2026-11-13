/**
 * Declares the virtual user modules the SDK imports.
 * 
 * The types are written as inline `import("...")` types on purpose.
 * Ambient module declarations can't reach another module through a
 * relative import statement (TS2439).
 */

declare module "virtual:wasp/user/tasks/operations" {
  export const getTasks: import("./server/operations/queries/index").RegisteredGetTasks;
}

declare module "virtual:wasp/user/tasks/operations" {
  export const getTaskStats: import("./server/operations/queries/index").RegisteredGetTaskStats;
}

declare module "virtual:wasp/user/tasks/operations" {
  export const deleteTask: import("./server/operations/actions/index").RegisteredDeleteTask;
}

declare module "virtual:wasp/user/tasks/operations" {
  export const createTask: import("./server/operations/actions/index").RegisteredCreateTask;
}
