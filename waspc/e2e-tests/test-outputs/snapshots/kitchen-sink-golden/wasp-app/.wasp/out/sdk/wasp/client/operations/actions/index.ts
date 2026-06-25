import { type ActionFor, createAction } from './core'
import type { CustomSignup_ext } from '../../../server/operations/actions/index.js'
import type { CreateTask_ext } from '../../../server/operations/actions/index.js'
import type { UpdateTaskIsDone_ext } from '../../../server/operations/actions/index.js'
import type { DeleteCompletedTasks_ext } from '../../../server/operations/actions/index.js'
import type { ToggleAllTasks_ext } from '../../../server/operations/actions/index.js'
import type { RequestUppercaseText_ext } from '../../../server/operations/actions/index.js'
import type { TestingAction_ext } from '../../../server/operations/actions/index.js'
import type { TaskToTaskUnspecified_ext } from '../../../server/operations/actions/index.js'
import type { TaskToTaskSatisfies_ext } from '../../../server/operations/actions/index.js'
import type { TaskToTaskSpecified_ext } from '../../../server/operations/actions/index.js'
import type { VoidToStringAuth_ext } from '../../../server/operations/actions/index.js'
import type { VoidToStringNoAuth_ext } from '../../../server/operations/actions/index.js'
import type { UnspecifiedToNumber_ext } from '../../../server/operations/actions/index.js'
import type { BoolToStringAuth_ext } from '../../../server/operations/actions/index.js'
import type { BoolToStringNoAuth_ext } from '../../../server/operations/actions/index.js'
import type { BoolToVoidNoAuth_ext } from '../../../server/operations/actions/index.js'
import type { BoolToVoidAuth_ext } from '../../../server/operations/actions/index.js'
import type { JsActionWithArgs_ext } from '../../../server/operations/actions/index.js'

// PUBLIC API
export const customSignup: ActionFor<CustomSignup_ext> = createAction<CustomSignup_ext>(
  'operations/custom-signup',
  [],
)

// PUBLIC API
export const createTask: ActionFor<CreateTask_ext> = createAction<CreateTask_ext>(
  'operations/create-task',
  ['Task'],
)

// PUBLIC API
export const updateTaskIsDone: ActionFor<UpdateTaskIsDone_ext> = createAction<UpdateTaskIsDone_ext>(
  'operations/update-task-is-done',
  ['Task'],
)

// PUBLIC API
export const deleteCompletedTasks: ActionFor<DeleteCompletedTasks_ext> = createAction<DeleteCompletedTasks_ext>(
  'operations/delete-completed-tasks',
  ['Task'],
)

// PUBLIC API
export const toggleAllTasks: ActionFor<ToggleAllTasks_ext> = createAction<ToggleAllTasks_ext>(
  'operations/toggle-all-tasks',
  ['Task'],
)

// PUBLIC API
export const requestUppercaseText: ActionFor<RequestUppercaseText_ext> = createAction<RequestUppercaseText_ext>(
  'operations/request-uppercase-text',
  ['UppercaseTextRequest'],
)

// PUBLIC API
export const testingAction: ActionFor<TestingAction_ext> = createAction<TestingAction_ext>(
  'operations/testing-action',
  [],
)

// PUBLIC API
export const taskToTaskUnspecified: ActionFor<TaskToTaskUnspecified_ext> = createAction<TaskToTaskUnspecified_ext>(
  'operations/task-to-task-unspecified',
  ['Task'],
)

// PUBLIC API
export const taskToTaskSatisfies: ActionFor<TaskToTaskSatisfies_ext> = createAction<TaskToTaskSatisfies_ext>(
  'operations/task-to-task-satisfies',
  ['Task'],
)

// PUBLIC API
export const taskToTaskSpecified: ActionFor<TaskToTaskSpecified_ext> = createAction<TaskToTaskSpecified_ext>(
  'operations/task-to-task-specified',
  ['Task'],
)

// PUBLIC API
export const voidToStringAuth: ActionFor<VoidToStringAuth_ext> = createAction<VoidToStringAuth_ext>(
  'operations/void-to-string-auth',
  ['Task'],
)

// PUBLIC API
export const voidToStringNoAuth: ActionFor<VoidToStringNoAuth_ext> = createAction<VoidToStringNoAuth_ext>(
  'operations/void-to-string-no-auth',
  ['Task'],
)

// PUBLIC API
export const unspecifiedToNumber: ActionFor<UnspecifiedToNumber_ext> = createAction<UnspecifiedToNumber_ext>(
  'operations/unspecified-to-number',
  ['Task'],
)

// PUBLIC API
export const boolToStringAuth: ActionFor<BoolToStringAuth_ext> = createAction<BoolToStringAuth_ext>(
  'operations/bool-to-string-auth',
  ['Task'],
)

// PUBLIC API
export const boolToStringNoAuth: ActionFor<BoolToStringNoAuth_ext> = createAction<BoolToStringNoAuth_ext>(
  'operations/bool-to-string-no-auth',
  ['Task'],
)

// PUBLIC API
export const boolToVoidNoAuth: ActionFor<BoolToVoidNoAuth_ext> = createAction<BoolToVoidNoAuth_ext>(
  'operations/bool-to-void-no-auth',
  ['Task'],
)

// PUBLIC API
export const boolToVoidAuth: ActionFor<BoolToVoidAuth_ext> = createAction<BoolToVoidAuth_ext>(
  'operations/bool-to-void-auth',
  ['Task'],
)

// PUBLIC API
export const jsActionWithArgs: ActionFor<JsActionWithArgs_ext> = createAction<JsActionWithArgs_ext>(
  'operations/js-action-with-args',
  ['Task'],
)
