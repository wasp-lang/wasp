import { HttpError } from "wasp/server";
import { dashboardUrl } from "wasp/server/mcp";
import type {
  CreateTask,
  DeleteTask,
  GetTasks,
  GetTaskStats,
} from "wasp/server/operations";
import type {
  CreateTaskInput,
  DeleteTaskInput,
  GetTasksInput,
  TaskOut,
  TasksOut,
} from "./schemas";

const toView = (task: {
  id: number;
  description: string;
  isDone: boolean;
  createdAt: Date;
}): TaskOut => ({
  id: task.id,
  description: task.description,
  isDone: task.isDone,
  createdAt: task.createdAt.toISOString(),
});

export const getTasks: GetTasks<GetTasksInput, TasksOut> = async (
  args,
  context,
) => {
  if (!context.user) throw new HttpError(401);
  const tasks = await context.entities.Task.findMany({
    where: { userId: context.user.id, isDone: args?.done },
    orderBy: { createdAt: "desc" },
  });
  return { tasks: tasks.map(toView) };
};

export const createTask: CreateTask<CreateTaskInput, TaskOut> = async (
  args,
  context,
) => {
  if (!context.user) throw new HttpError(401);
  // context.mcp is set when an MCP client calls this as a tool.
  const via = context.mcp ? " (via " + context.mcp.connection.clientName + ")" : "";
  const task = await context.entities.Task.create({
    data: { description: args.description + via, userId: context.user.id },
  });
  return toView(task);
};

export const getTaskStats: GetTaskStats<
  GetTasksInput,
  { open: number; done: number; dashboardUrl: string; newestTaskUrl: string | null }
> = async (_args, context) => {
  if (!context.user) throw new HttpError(401);
  const where = { userId: context.user.id };
  const newest = await context.entities.Task.findFirst({
    where,
    orderBy: { createdAt: "desc" },
  });
  return {
    open: await context.entities.Task.count({ where: { ...where, isDone: false } }),
    done: await context.entities.Task.count({ where: { ...where, isDone: true } }),
    // Links back into the app, built from route names.
    dashboardUrl: dashboardUrl("RootRoute", { search: { view: "stats" } }),
    newestTaskUrl: newest
      ? dashboardUrl("TaskRoute", { params: { id: newest.id } })
      : null,
  };
};

export const deleteTask: DeleteTask<DeleteTaskInput, { id: number }> = async (
  args,
  context,
) => {
  if (!context.user) throw new HttpError(401);
  const { count } = await context.entities.Task.deleteMany({
    where: { id: args.id, userId: context.user.id },
  });
  if (count === 0)
    throw new HttpError(404, "Task " + args.id + " not found.", {
      nextStep: "Use list_tasks to find valid IDs.",
    });
  return { id: args.id };
};