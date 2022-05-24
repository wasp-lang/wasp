import { AbilityBuilder, Ability } from '@casl/ability';
import { PrismaAbility } from '@casl/prisma';

export async function taskAbility(args, context) {
  const { can, cannot, rules } = new AbilityBuilder(PrismaAbility);
  const task = await context.entities.Task.findUnique({
    where: { id: args.taskId }
  });

  if (context.user.id === task.userId) {
    can('operateOn', 'updateTask');
  }

  return new Ability(rules);
}
