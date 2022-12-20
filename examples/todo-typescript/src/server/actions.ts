import HttpError from '@wasp/core/HttpError.js';
import { Context, Task } from './serverTypes'

type createArgs = { description: Task['description'] };

export const createTask = async ({ description }: createArgs, context: Context) => {
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

type updateArgs = { taskId: Task['id']; isDone: Task['isDone'] };

export const updateTask = async ({ taskId, isDone }: updateArgs, context: Context) => {
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
