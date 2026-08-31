import { type ActionFor, createAction } from "./core";
import type {
  RegisteredCustomSignup,
  RegisteredCreateTask,
  RegisteredUpdateTaskIsDone,
  RegisteredDeleteCompletedTasks,
  RegisteredToggleAllTasks,
  RegisteredRequestUppercaseText,
  RegisteredTestingAction,
  RegisteredTaskToTaskUnspecified,
  RegisteredTaskToTaskSatisfies,
  RegisteredTaskToTaskSpecified,
  RegisteredVoidToStringAuth,
  RegisteredVoidToStringNoAuth,
  RegisteredUnspecifiedToNumber,
  RegisteredBoolToStringAuth,
  RegisteredBoolToStringNoAuth,
  RegisteredBoolToVoidNoAuth,
  RegisteredBoolToVoidAuth,
  RegisteredJsActionWithArgs,
} from "../../../server/operations/actions/index";

// PUBLIC API
export const customSignup: ActionFor<RegisteredCustomSignup> = createAction<RegisteredCustomSignup>(
  "operations/custom-signup",
  [],
)

// PUBLIC API
export const createTask: ActionFor<RegisteredCreateTask> = createAction<RegisteredCreateTask>(
  "operations/create-task",
  ['Task'],
)

// PUBLIC API
export const updateTaskIsDone: ActionFor<RegisteredUpdateTaskIsDone> = createAction<RegisteredUpdateTaskIsDone>(
  "operations/update-task-is-done",
  ['Task'],
)

// PUBLIC API
export const deleteCompletedTasks: ActionFor<RegisteredDeleteCompletedTasks> = createAction<RegisteredDeleteCompletedTasks>(
  "operations/delete-completed-tasks",
  ['Task'],
)

// PUBLIC API
export const toggleAllTasks: ActionFor<RegisteredToggleAllTasks> = createAction<RegisteredToggleAllTasks>(
  "operations/toggle-all-tasks",
  ['Task'],
)

// PUBLIC API
export const requestUppercaseText: ActionFor<RegisteredRequestUppercaseText> = createAction<RegisteredRequestUppercaseText>(
  "operations/request-uppercase-text",
  ['UppercaseTextRequest'],
)

// PUBLIC API
export const testingAction: ActionFor<RegisteredTestingAction> = createAction<RegisteredTestingAction>(
  "operations/testing-action",
  [],
)

// PUBLIC API
export const taskToTaskUnspecified: ActionFor<RegisteredTaskToTaskUnspecified> = createAction<RegisteredTaskToTaskUnspecified>(
  "operations/task-to-task-unspecified",
  ['Task'],
)

// PUBLIC API
export const taskToTaskSatisfies: ActionFor<RegisteredTaskToTaskSatisfies> = createAction<RegisteredTaskToTaskSatisfies>(
  "operations/task-to-task-satisfies",
  ['Task'],
)

// PUBLIC API
export const taskToTaskSpecified: ActionFor<RegisteredTaskToTaskSpecified> = createAction<RegisteredTaskToTaskSpecified>(
  "operations/task-to-task-specified",
  ['Task'],
)

// PUBLIC API
export const voidToStringAuth: ActionFor<RegisteredVoidToStringAuth> = createAction<RegisteredVoidToStringAuth>(
  "operations/void-to-string-auth",
  ['Task'],
)

// PUBLIC API
export const voidToStringNoAuth: ActionFor<RegisteredVoidToStringNoAuth> = createAction<RegisteredVoidToStringNoAuth>(
  "operations/void-to-string-no-auth",
  ['Task'],
)

// PUBLIC API
export const unspecifiedToNumber: ActionFor<RegisteredUnspecifiedToNumber> = createAction<RegisteredUnspecifiedToNumber>(
  "operations/unspecified-to-number",
  ['Task'],
)

// PUBLIC API
export const boolToStringAuth: ActionFor<RegisteredBoolToStringAuth> = createAction<RegisteredBoolToStringAuth>(
  "operations/bool-to-string-auth",
  ['Task'],
)

// PUBLIC API
export const boolToStringNoAuth: ActionFor<RegisteredBoolToStringNoAuth> = createAction<RegisteredBoolToStringNoAuth>(
  "operations/bool-to-string-no-auth",
  ['Task'],
)

// PUBLIC API
export const boolToVoidNoAuth: ActionFor<RegisteredBoolToVoidNoAuth> = createAction<RegisteredBoolToVoidNoAuth>(
  "operations/bool-to-void-no-auth",
  ['Task'],
)

// PUBLIC API
export const boolToVoidAuth: ActionFor<RegisteredBoolToVoidAuth> = createAction<RegisteredBoolToVoidAuth>(
  "operations/bool-to-void-auth",
  ['Task'],
)

// PUBLIC API
export const jsActionWithArgs: ActionFor<RegisteredJsActionWithArgs> = createAction<RegisteredJsActionWithArgs>(
  "operations/js-action-with-args",
  ['Task'],
)
