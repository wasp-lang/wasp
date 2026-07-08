import { type Spec, action, page, query, route } from "@wasp.sh/spec";
import {
  createTask,
  deleteCompletedTasks,
  updateTaskStatus,
} from "./actions" with { type: "ref" };
import { getTasks } from "./queries" with { type: "ref" };
import { TasksPage } from "./TasksPage" with { type: "ref" };

// We also want to use it in the `auth` configuration, so we export it.
export const tasksRoute = route(
  "TasksRoute",
  "/",
  page(TasksPage, { authRequired: true }),
);

export const tasksSpec: Spec = [
  tasksRoute,
  query(getTasks, { entities: ["Task", "Tag"] }),
  action(createTask, { entities: ["Task"] }),
  action(updateTaskStatus, { entities: ["Task"] }),
  action(deleteCompletedTasks, { entities: ["Task"] }),
];
