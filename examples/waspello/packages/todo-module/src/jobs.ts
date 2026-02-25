import type { OperationContext } from "wasp/server/module";
import { moduleConfig } from "./config.js";

const { todoEntityName } = moduleConfig;

export async function cleanDoneTodos(
  _args: unknown,
  context: OperationContext
): Promise<void> {
  const { count } = await context.entities[todoEntityName].deleteMany({
    where: { isDone: true },
  });
  const remaining = await context.entities[todoEntityName].count();
  const now = new Date().toISOString();
  console.log(
    `[todo-module] cleanDoneTodos executed at ${now} — removed ${count} done todo(s), ${remaining} remaining`
  );
}
