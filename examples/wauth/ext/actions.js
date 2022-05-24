import HttpError from '@wasp/core/HttpError.js'
import userAbility from './userAbility.js';
import { taskAbility } from './operationAbilities.js';
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

export const updateTask = async (args, context) => {
  const { taskId, data } = args;
  const ability1 = userAbility(context.user);
  const ability2 = await taskAbility(args, context);
  const task = await context.entities.Task.findUnique({
    where: { id: taskId }
  });
  // NOTE: Must use `subject()` since our task no longer has Prisma prototype chain.
  if (ability1.cannot('update', subject('Task', task))) {
    throw new HttpError(403, 'Update check failed');
  }
  // This is an alternative, custom approach example, that duplicates above.
  // NOTE: This would be an easy convention we could automatically check for users.
  if (ability2.cannot('operateOn', 'updateTask')) {
    throw new HttpError(403, 'Operate check failed');
  }

  // NOTE: Has to be `updateMany` instead of `update` since
  // CASL Prisma assumes WhereInput and not WhereUniqueInput.
  return context.entities.Task.updateMany({
    where: {
      AND: [
        accessibleBy(ability1).Task,
        { id: taskId }
      ]
    },
    data: { isDone: data.isDone }
  });
}
