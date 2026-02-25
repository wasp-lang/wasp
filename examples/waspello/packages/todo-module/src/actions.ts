import { z } from "zod";
import type { AuthOperationContext } from "wasp/server/module";
import type { Todo } from "./store.js";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { todoEntityName, userForeignKey } = moduleConfig;

const createTodoInput = z.object({ text: z.string().min(1) });

export async function createTodo(
  args: unknown,
  context: AuthOperationContext,
): Promise<Todo> {
  const user = requireUser(context);
  const { text } = createTodoInput.parse(args);
  return context.entities[todoEntityName].create({
    data: {
      text,
      isDone: false,
      ...(userForeignKey && { [userForeignKey]: user.id }),
    },
  });
}

const updateTodoInput = z.object({
  id: z.number(),
  text: z.string().optional(),
  isDone: z.boolean().optional(),
});

export async function updateTodo(
  args: unknown,
  context: AuthOperationContext,
): Promise<Todo> {
  const user = requireUser(context);
  const { id, ...data } = updateTodoInput.parse(args);
  return context.entities[todoEntityName].update({
    where: { id, ...(userForeignKey && { [userForeignKey]: user.id }) },
    data,
  });
}

const deleteTodoInput = z.object({ id: z.number() });

export async function deleteTodo(
  args: unknown,
  context: AuthOperationContext,
): Promise<Todo> {
  const user = requireUser(context);
  const { id } = deleteTodoInput.parse(args);
  return context.entities[todoEntityName].delete({
    where: { id, ...(userForeignKey && { [userForeignKey]: user.id }) },
  });
}
