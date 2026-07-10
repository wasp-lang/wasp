import { type Spec, action, page, query } from "@wasp.sh/spec";

import {
  createTask,
  deleteCompletedTasks,
  updateTaskStatus,
} from "./actions" with { type: "ref" };
import { getTasks } from "./queries" with { type: "ref" };
import { TasksPage } from "./TasksPage" with { type: "ref" };

export const tasksPage = page(TasksPage, { authRequired: true });

export const tasksOperations = [
  query(getTasks, { entities: ["Task", "Tag"] }),
  action(createTask, { entities: ["Task"] }),
  action(updateTaskStatus, { entities: ["Task"] }),
  action(deleteCompletedTasks, { entities: ["Task"] }),
];

export const tasksSpec: Spec = [tasksPage, tasksOperations];
