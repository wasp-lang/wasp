import { AbilityBuilder, Ability } from '@casl/ability';
import { PrismaAbility } from '@casl/prisma';

// Wasp behind the scenes version that wraps user version.
export async function _taskAbility(operationName, args, context) {
  const builder = new AbilityBuilder(PrismaAbility);
  await taskAbility(args, context, () => { builder.can('execute', operationName); }, () => { builder.cannot('execute', operationName); });
  return builder.build();
}

// The interface the user can define operation checks on.
// Used by importing this fn into a check for some operation.
// NOTE: All these checks will introduce an extra DB call, unless we let them
// put them back into the context/args somehow.
export async function taskAbility(args, context, allow, deny) {
  const task = await context.entities.Task.findUnique({
    where: { id: args.taskId }
  });

  if (context.user.id === task.userId) {
    allow();
  }
}
