import { type Spec, action, page, query, route } from "@wasp.sh/spec";
import {
  createTask,
  deleteCompletedTasks,
  updateTaskStatus,
} from "./actions" with { type: "ref" };
import { getTasks } from "./queries" with { type: "ref" };
import { TasksPage } from "./TasksPage" with { type: "ref" };

export const tasksSpec: Spec = [
  route("TasksRoute", "/", page(TasksPage, { authRequired: true })),
  query(getTasks, { entities: ["Task", "Tag"] }),
  action(createTask, { entities: ["Task"] }),
  action(updateTaskStatus, { entities: ["Task"] }),
  action(deleteCompletedTasks, { entities: ["Task"] }),
];
