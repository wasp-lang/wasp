import HttpError from '@wasp/core/HttpError.js'
import userAbility from './userAbility.js';
import { _taskAbility } from './operationAbilities.js';
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

// Wasp behind the scenes version that wraps user version.
export const _updateTask = async (args, context) => {
  const ability = await _taskAbility('updateTask', args, context);
  if (ability.cannot('execute', 'updateTask')) {
    throw new HttpError(403, 'Operate check failed');
  }

  return updateTask(args, context);
}

// User would write their operations knowing the operation checks already happened.
export const updateTask = async (args, context) => {
  const { taskId, data } = args;
  const ability = userAbility(context.user);
  const task = await context.entities.Task.findUnique({
    where: { id: taskId }
  });

  // NOTE: Must use `subject()` since our task no longer has Prisma prototype chain.
  if (ability.cannot('update', subject('Task', task))) {
    throw new HttpError(403, 'Update check failed');
  }

  // NOTE: Has to be `updateMany` instead of `update` since
  // CASL Prisma assumes WhereInput and not WhereUniqueInput.
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
