import HttpError from '@wasp/core/HttpError.js'
import userAbility from './userAbility.js';
import { accessibleBy } from '@casl/prisma';
import { subject } from '@casl/ability';

export const createTask = async ({ description }, context) => {
  const ability = userAbility(context.user);
  if (ability.cannot('create', 'Task')) {
    throw new HttpError(403);
  }

  return context.entities.Task.create({
    data: {
      description,
      user: { connect: { id: context.user.id } }
    }
  });
}

export const updateTask = async ({ taskId, data }, context) => {
  const ability = userAbility(context.user);
  const task = await context.entities.Task.findUnique({
    where: { id: taskId }
  });
  if (ability.cannot('update', subject('Task', task))) {
    throw new HttpError(403);
  }

  // For some reason has to be `updateMany` instead of `update`?
  return context.entities.Task.updateMany({
    where: {
      AND: [
        accessibleBy(ability).Task,
        { id: taskId }
      ]
    },
    data: { isDone: data.isDone }
  });
}
