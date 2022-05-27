import HttpError from '../core/HttpError.js'
import userAbility from './userAbility.js';
import { accessibleBy } from '@casl/prisma';

export const getTasksOld = async (args, context) => {
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

// Example of doing checks via Proxy objects.
export const getTasks = async (_args, context) => {
  setupProxy(context, 'Task'); // Can set these up automatically by looking at entities[] in .wasp file.
  return context.checked.Task.findMany({ where: { userId: context.user.id } });
}

function setupProxy(context, entityName) {
  const ability = userAbility(context.user);
  const entity = context.entities[entityName];

  const readFunctions = ["findUnique", "findFirst", "findMany"];

  const entityProxy = {
    get: function (target, prop) {

      if (typeof target[prop] === 'function') {
        if (readFunctions.includes(prop)) {
          console.log("Before read check")
          if (ability.cannot('read', entityName)) {
            throw new Error("Read check failed!");
          }
        } // and so on for other verbs.

        return target[prop].bind(target);
      }
    }
  };

  context.checked = { ...context.checked, [entityName]: new Proxy(entity, entityProxy) };
}
