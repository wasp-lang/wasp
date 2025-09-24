import type { Task } from "wasp/entities";
import { HttpError } from "wasp/server";
import type { CreateTask, UpdateTask } from "wasp/server/operations";

type CreateTaskPayload = Pick<Task, "description">;

export const createTask: CreateTask<CreateTaskPayload, Task> = async (
  args,
  context,
) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.create({
    data: {
      description: args.description,
      user: { connect: { id: context.user.id } },
    },
  });
};

type UpdateTaskPayload = Pick<Task, "id" | "isDone">;

export const updateTask: UpdateTask<
  UpdateTaskPayload,
  { count: number }
> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.updateMany({
    where: { id: args.id, user: { id: context.user.id } },
    data: { isDone: args.isDone },
  });
};
