import HttpError from "@wasp/core/HttpError.js";
import type { CreateTask, UpdateTask, DeleteTasks } from "@wasp/actions/types";
import type { Task } from "@wasp/entities";

type CreateArgs = Pick<Task, "description">;

export const createTask: CreateTask<CreateArgs, Task> = async (
  { description },
  context
) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Task.create({
    data: {
      description,
      user: { connect: { id: context.user.id } },
    },
  });
};

type UpdateArgs = Pick<Task, "id" | "isDone">;

export const updateTask: UpdateTask<UpdateArgs> = async (
  { id, isDone },
  context
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

export const deleteTasks: DeleteTasks<Task["id"][]> = async (
  idsToDelete,
  context
) => {
  return context.entities.Task.deleteMany({
    where: {
      id: {
        in: idsToDelete,
      },
    },
  });
};
