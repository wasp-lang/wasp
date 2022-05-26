import HttpError from '@wasp/core/HttpError.js'
import userAbility from './userAbility.js';
import { accessibleBy } from '@casl/prisma';

export const getTasks = async (args, context) => {
  const ability = userAbility(context.user);
  if (ability.cannot('read', 'Task')) {
    throw new HttpError(403);
  }

  // But he probably doesn't want to get all the tasks!
  // Instead, what he wants is to get their own tasks, right?
  // Because they might have access to tasks of some other person, in theory.
  // So their intention is not to get all the tasks they can access, their intention is to get their tasks.
  return context.entities.Task.findMany({
    where: accessibleBy(ability).Task
  });
}
