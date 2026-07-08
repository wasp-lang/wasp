import { getRandomQuote } from "quote-lib";
import type { AddRandomTodo } from "wasp/server/operations";

type HostUser = {
  id: number;
};

type HostTaskDelegate = {
  create(args: {
    data: {
      description: string;
      user: {
        connect: { id: HostUser["id"] };
      };
    };
  }): Promise<unknown>;
};

type HostContext = {
  user?: HostUser;
  entities: {
    Task: HostTaskDelegate;
  };
};

export const addRandomTodo: AddRandomTodo<void, void, HostContext> = async (
  _args,
  context,
) => {
  if (!context.user) {
    throw new Error("Log in before adding TODOs from this module.");
  }

  const Task = context.entities.Task;
  const quote = getRandomQuote();
  const author = quote.author ? ` - ${quote.author}` : "";

  await Task.create({
    data: {
      description: `${quote.text}${author}`,
      user: {
        connect: { id: context.user.id },
      },
    },
  });
};
