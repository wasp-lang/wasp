import { HttpError } from "wasp/server";
import type { GetOldestTask } from "wasp/server/operations";

// Default-exported on purpose, it is the only operation in this app that Wasp
// references through a default import.
const getOldestTask = (async (_args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.findFirst({
    where: { user: { id: context.user.id } },
    orderBy: { id: "asc" },
  });
}) satisfies GetOldestTask<void>;

export default getOldestTask;
