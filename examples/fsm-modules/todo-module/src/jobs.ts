import type { OperationContext } from "wasp/server/module";
import type { Todo } from "./store.js";

type Entities = { Todo: Todo };

export async function cleanDoneTodos(
  _args: unknown,
  context: OperationContext<Entities>
): Promise<void> {
  const { count } = await context.entities.Todo.deleteMany({
    where: { isDone: true },
  });
  const remaining = await context.entities.Todo.count();
  const now = new Date().toISOString();
  console.log(
    `[todo-module] cleanDoneTodos executed at ${now} — removed ${count} done todo(s), ${remaining} remaining`
  );
}
