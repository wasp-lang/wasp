import type { QueryFor } from '@wasp/lib-sdk-core/browser'
import { createQuery } from "./core";
import type {
  RegisteredGetTasks,
} from "../../../server/operations/queries/index";

// PUBLIC API
export const getTasks: QueryFor<RegisteredGetTasks> = createQuery<RegisteredGetTasks>(
  "operations/get-tasks",
  ["Task"],
)
