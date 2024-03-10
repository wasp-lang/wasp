import { HttpError } from "wasp/server";

export const getListsAndCards = async (args, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.List.findMany({
    // We want to make sure user can get only his own info.
    where: { user: { id: context.user.id } },
    include: { cards: true }
  })
}

export const createList = async ({ name, pos }, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.List.create({
    data: {
      name,
      pos,
      user: { connect: { id: context.user.id } }
    }
  })
}

export const updateList = async ({ listId, data }, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.List.updateMany({
    where: { id: listId, user: { id: context.user.id } },
    data: {
      name: data.name,
      pos: data.pos
    }
  })
}

export const deleteList = async ({ listId }, context) => {
  if (!context.user) { throw new HttpError(403) }

  // We make sure that user is not trying to delete somebody else's list.
  const list = await context.entities.List.findUnique({
    where: { id: listId }
  })
  if (list.userId !== context.user.id) { throw new HttpError(403) }

  // First delete all the cards that are in the list we want to delete.
  await context.entities.Card.deleteMany({
    where: { listId }   
  })

  await context.entities.List.delete({
    where: { id: listId }
  })
}

export const createListCopy = async ({ listId, pos }, context) => {
  if (!context.user) {
    throw new HttpError(403);
  }

  // Check if user owns the list.
  const list = await context.entities.List.findFirst({
    where: { id: listId, user: { id: context.user.id } },
  });

  if (!list) {
    throw new HttpError(403);
  }

  // Create a new list.
  const newList = await context.entities.List.create({
    data: {
      name: list.name + " (copy)",
      pos: pos,
      user: { connect: { id: context.user.id } },
    },
  });

  // Create copies of all the cards in the list.
  const cards = await context.entities.Card.findMany({
    where: { listId: list.id },
  });
  await Promise.all(
    cards.map((card) =>
      context.entities.Card.create({
        data: {
          title: card.title,
          pos: card.pos,
          list: { connect: { id: newList.id } },
          author: { connect: { id: context.user.id } },
        },
      })
    )
  );

  return newList;
};
