import { parseArgs } from "node:util";
import type { DeleteDoneTasks } from "wasp/server/tasks";

// Run with `wasp task run deleteDoneTasks [--dry-run]`.
export const deleteDoneTasks: DeleteDoneTasks = async (args, context) => {
  const { values } = parseArgs({
    args,
    options: { "dry-run": { type: "boolean", default: false } },
  });

  if (values["dry-run"]) {
    const count = await context.entities.Task.count({
      where: { isDone: true },
    });
    console.log(`Would delete ${count} done tasks.`);
    return;
  }

  const { count } = await context.entities.Task.deleteMany({
    where: { isDone: true },
  });
  console.log(`Deleted ${count} done tasks.`);
};
