import { getRandomQuote } from "quote-lib";
import type { AddRandomTodo } from "wasp/server/operations";
import type { HostContext } from "./types";

export const addRandomTodo: AddRandomTodo<void, void, HostContext> = async (
  _args,
  context,
) => {
  if (!context.user) {
    throw new Error("Log in before adding TODOs from this module.");
  }

  const quote = getRandomQuote();
  const author = quote.author ? ` - ${quote.author}` : "";

  await context.entities.Task.create({
    data: {
      description: `${quote.text}${author}`,
      user: {
        connect: { id: context.user.id },
      },
    },
  });
};
