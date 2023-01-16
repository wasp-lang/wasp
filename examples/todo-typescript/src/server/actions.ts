import HttpError from '@wasp/core/HttpError.js';
import { Context, Task } from './serverTypes'

type CreateArgs = { description: Task['description'] };

export async function createTask({ description }: CreateArgs, context: Context) {
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

type UpdateArgs = { taskId: Task['id']; isDone: Task['isDone'] };

export async function updateTask({ taskId, isDone }: UpdateArgs, context: Context) {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Task.updateMany({
    where: {
      id: taskId,
      user: { id: context.user.id },
    },
    data: { isDone },
  });
};
