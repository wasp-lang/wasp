import { type Tag, type Task } from "wasp/entities";
import { HttpError } from "wasp/server";
import {
  DeleteCompletedTasks,
  type CreateTask,
  type UpdateTaskStatus,
} from "wasp/server/operations";

type CreateTaskArgs = Pick<Task, "description"> & {
  tagIds: Tag["id"][];
};

export const createTask: CreateTask<CreateTaskArgs, Task> = async (
  { description, tagIds },
  context,
) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Task.create({
    data: {
      description,
      isDone: false,
      user: {
        connect: {
          id: context.user.id,
        },
      },
      tags: {
        connect: tagIds.map((tag) => ({
          id: tag,
        })),
      },
    },
  });
};

type UpdateTaskStatusArgs = Pick<Task, "id" | "isDone">;

export const updateTaskStatus: UpdateTaskStatus<UpdateTaskStatusArgs> = async (
  { id, isDone },
  context,
) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Task.update({
    where: {
      id,
    },
    data: { isDone },
  });
};

export const deleteCompletedTasks: DeleteCompletedTasks = async (
  _args,
  context,
) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Task.deleteMany({
    where: {
      userId: context.user.id,
      isDone: true,
    },
  });
};
