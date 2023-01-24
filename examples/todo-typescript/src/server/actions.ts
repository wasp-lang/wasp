import HttpError from '@wasp/core/HttpError.js';
import { Context, Task } from './serverTypes'

type CreateArgs = Pick<Task, 'description'>;

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

// type UpdateArgs = { taskId: Task['id']; isDone: Task['isDone'] };
type UpdateArgs = Pick<Task, 'id' | 'isDone'>;

export async function updateTask({ id, isDone }: UpdateArgs, context: Context) {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Task.updateMany({
    where: {
      id,
      user: { id: context.user.id },
    },
    data: { isDone },
  });
};
