import type { ActionFor } from '@wasp/lib-sdk-core/browser'
import { createAction } from "./core";
import type {
  RegisteredDeleteTask,
  RegisteredCreateTask,
} from "../../../server/operations/actions/index";

// PUBLIC API
export const deleteTask: ActionFor<RegisteredDeleteTask> = createAction<RegisteredDeleteTask>(
  "operations/delete-task",
  ["Task"],
)

// PUBLIC API
export const createTask: ActionFor<RegisteredCreateTask> = createAction<RegisteredCreateTask>(
  "operations/create-task",
  ["Task"],
)
