import { Idea } from "@wasp/entities";
import { GetIdeas } from "@wasp/queries/types";
import { CreateIdea, ToggleAgreement } from "@wasp/actions/types";

export const getIdeas: GetIdeas<
  {},
  {
    id: number;
    idea: string;
    agreedUsers: string[];
    user: { username: string };
  }[]
> = async (args, context) => {
  const ideas = await context.entities.Idea.findMany({
    orderBy: {
      agreedUsers: {
        _count: "desc",
      }
    },
    select: {
      id: true,
      idea: true,
      agreedUsers: {
        select: {
          username: true,
        },
      },
      user: {
        select: {
          username: true,
        },
      },
    },
  });
  return ideas.map((idea) => {
    return {
      id: idea.id,
      idea: idea.idea,
      agreedUsers: idea.agreedUsers.map((user) => user.username),
      user: idea.user,
    };
  });
};

export const toggleAgreement: ToggleAgreement<{ id: number }, boolean> = async (
  args,
  context
) => {
  if (!context.user) {
    throw new Error("You must be logged in to agree with an idea.");
  }
  const idea = await context.entities.Idea.findUnique({
    where: { id: args.id },
    include: { agreedUsers: true },
  });
  if (!idea) {
    throw new Error("Idea not found.");
  }
  const agreedUserIds = idea.agreedUsers.map((user) => user.id);
  if (agreedUserIds.includes(context.user.id)) {
    // User has already agreed with this idea, so remove their agreement.
    await context.entities.Idea.update({
      where: { id: args.id },
      data: {
        agreedUsers: {
          disconnect: { id: context.user.id },
        },
      },
    });
  } else {
    // User has not yet agreed with this idea, so add their agreement.
    await context.entities.Idea.update({
      where: { id: args.id },
      data: {
        agreedUsers: {
          connect: { id: context.user.id },
        },
      },
    });
  }
  return true;
};


export const createIdea: CreateIdea<{ idea: string }, Idea> = async (args, context) => {
    if (!context.user) {
      throw new Error("You must be logged in to create an idea.");
    }
    return context.entities.Idea.create({
        data: {
            idea: args.idea,
            user: {
                connect: { id: context.user.id },
            },
        },
    });
}