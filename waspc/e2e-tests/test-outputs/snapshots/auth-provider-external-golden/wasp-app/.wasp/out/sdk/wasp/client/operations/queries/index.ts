import { type QueryFor, createQuery } from "./core";
import type {
  RegisteredGetMyTasks,
} from "../../../server/operations/queries/index";

// PUBLIC API
export const getMyTasks: QueryFor<RegisteredGetMyTasks> = createQuery<RegisteredGetMyTasks>(
  "operations/get-my-tasks",
  ['Task'],
)

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from "./core"
