import { type ActionFor, createAction } from "./core";
import type {
  RegisteredCreateTask,
} from "../../../server/operations/actions/index";

// PUBLIC API
export const createTask: ActionFor<RegisteredCreateTask> = createAction<RegisteredCreateTask>(
  "operations/create-task",
  ['Task'],
)
