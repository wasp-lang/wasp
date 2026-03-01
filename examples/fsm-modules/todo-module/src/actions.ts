import { z } from "zod";
import type { AuthenticatedAction } from "wasp/server/module";
import type { Todo } from "./store.js";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { userForeignKey } = moduleConfig;

type Entities = { Todo: Todo };

const createTodoInput = z.object({ text: z.string().min(1) });

export const createTodo: AuthenticatedAction<Entities, unknown, Todo> = async (
  args,
  context,
) => {
  const user = requireUser(context);
  const { text } = createTodoInput.parse(args);
  return context.entities.Todo.create({
    data: {
      text,
      isDone: false,
      ...(userForeignKey && { [userForeignKey]: user.id }),
    },
  });
};

const updateTodoInput = z.object({
  id: z.number(),
  text: z.string().optional(),
  isDone: z.boolean().optional(),
});

export const updateTodo: AuthenticatedAction<Entities, unknown, Todo> = async (
  args,
  context,
) => {
  const user = requireUser(context);
  const { id, ...data } = updateTodoInput.parse(args);
  return context.entities.Todo.update({
    where: { id, ...(userForeignKey && { [userForeignKey]: user.id }) },
    data,
  });
};

const deleteTodoInput = z.object({ id: z.number() });

export const deleteTodo: AuthenticatedAction<Entities, unknown, Todo> = async (
  args,
  context,
) => {
  const user = requireUser(context);
  const { id } = deleteTodoInput.parse(args);
  return context.entities.Todo.delete({
    where: { id, ...(userForeignKey && { [userForeignKey]: user.id }) },
  });
};
