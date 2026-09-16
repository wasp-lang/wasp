import { type QueryFor, createQuery } from "./core";
import type {
  RegisteredGetMyTasks,
  RegisteredGetAdminReport,
} from "../../../server/operations/queries/index";

// PUBLIC API
export const getMyTasks: QueryFor<RegisteredGetMyTasks> = createQuery<RegisteredGetMyTasks>(
  "operations/get-my-tasks",
  ['Task'],
)

// PUBLIC API
export const getAdminReport: QueryFor<RegisteredGetAdminReport> = createQuery<RegisteredGetAdminReport>(
  "operations/get-admin-report",
  ['Task'],
)

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from "./core"
