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
  setupProxy(context, 'Task');
  const ability = await _taskAbility('updateTask', args, context);
  if (ability.cannot('execute', 'updateTask')) {
    throw new HttpError(403, 'Operate check failed');
  }

  return updateTask(args, context);
}

// User would write their operations knowing the operation checks already happened.
export const updateTask = async (args, context) => {
  // Can be update or updateMany.
  return context.checked.Task.update({
    where: { id: args.taskId },
    data: { isDone: args.data.isDone }
  });
}

// POC for intercepting and patching the where clauses for update calls.
function setupProxy(context, entityName) {
  const ability = userAbility(context.user);
  const entity = context.entities[entityName];

  const entityProxy = {

    get: function (target, prop) {
      if (typeof target[prop] === 'function') {
        switch (prop) {
          // Showing how we can patch the where.
          case "updateMany":
            return function (args) {
              console.log(args);
              const checkedWhere = {
                AND: [
                  accessibleBy(ability)[entityName],
                  (args.where || {})
                ]
              }
              args.where = checkedWhere;
              return target[prop].apply(target, [args]);
            }
          // Showing how we can do a check _first_. Would likely need to wrap in a transaction though, right?
          case "update":
            return async function (args) {
              console.log(args);
              const entity = await target["findUnique"].apply(target, [{ where: args.where }]);
              // NOTE: Must use `subject()` since our task no longer has Prisma prototype chain.
              if (ability.cannot('update', subject('Task', entity))) {
                throw new HttpError(403, 'Update check failed');
              }
              return target[prop].apply(target, [args]);
            }
        }
      }
    }
  };

  context.checked = { ...context.checked, [entityName]: new Proxy(entity, entityProxy) };
}
