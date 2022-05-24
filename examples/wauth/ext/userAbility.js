import { AbilityBuilder, Ability } from '@casl/ability';
import { PrismaAbility } from '@casl/prisma';

export default function userAbility(user) {
  const { can, cannot, rules } = new AbilityBuilder(PrismaAbility);

  if (user) {
    can('create', 'Task');
    can(['read', 'update'], 'Task', { userId: user.id });
  }

  return new Ability(rules);
}
