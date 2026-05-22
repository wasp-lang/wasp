import { action, page, query, route, type Part } from "@wasp.sh/spec";

import {
  createTask,
  deleteCompletedTasks,
  toggleAllTasks,
  updateTaskIsDone,
} from "./actions" with { type: "ref" };
import { SerializationPage } from "./pages/SerializationPage" with { type: "ref" };
import { TaskDetailPage } from "./pages/TaskDetailPage" with { type: "ref" };
import { TasksPage } from "./pages/TasksPage" with { type: "ref" };
import {
  getNumTasks,
  getSerializedObjects,
  getTask,
  getTasks,
} from "./queries" with { type: "ref" };

export const operations: Part[] = [
  route("TasksRoute", "/tasks", page(TasksPage, { authRequired: true })),
  route(
    "TaskRoute",
    "/tasks/:id",
    page(TaskDetailPage, { authRequired: true }),
  ),
  query(getTasks, { entities: ["Task"] }),
  query(getNumTasks, { entities: ["Task"], auth: false }),
  query(getTask, { entities: ["Task"] }),
  action(createTask, { entities: ["Task"] }),
  action(updateTaskIsDone, { entities: ["Task"] }),
  action(deleteCompletedTasks, { entities: ["Task"] }),
  action(toggleAllTasks, { entities: ["Task"] }),
  query(getSerializedObjects),
  route("SerializationRoute", "/serialization", page(SerializationPage)),
];
