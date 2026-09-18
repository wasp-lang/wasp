import type { MergeUsersFn } from "wasp/server/auth";

/**
 * What merging two accounts means for THIS app: the tasks of the account that
 * goes away move to the one that stays. Wasp cannot know that, so it asks.
 *
 * Runs inside the merge transaction, before Wasp moves the logins and deletes
 * `from`. Use the given `prisma`, so these writes roll back with the merge.
 */
export const mergeUsers: MergeUsersFn = async ({ from, into, prisma }) => {
  await prisma.task.updateMany({
    where: { userId: from.id },
    data: { userId: into.id },
  });
};
