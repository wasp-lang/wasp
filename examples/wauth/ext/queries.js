import HttpError from '@wasp/core/HttpError.js'
import userAbility from './userAbility.js';
import { accessibleBy } from '@casl/prisma';

export const getTasks = async (args, context) => {
  const ability = userAbility(context.user);
  if (ability.cannot('read', 'Task')) {
    throw new HttpError(403);
  }

  return context.entities.Task.findMany({
    where: accessibleBy(ability).Task
  });
}
