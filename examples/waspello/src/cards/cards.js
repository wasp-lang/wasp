import { HttpError } from "wasp/server";

export const createCard = async ({ title, listId, pos }, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.Card.create({
    data: {
      title,
      pos,
      list: { connect: { id: listId } },
      author: { connect: { id: context.user.id } }
    }
  })
}

export const updateCard = async ({ cardId, data }, context) => {
  if (!context.user) { throw new HttpError(403) }

  // Check if user owns the card.
  const card = await context.entities.Card.findFirst({
    where: { id: cardId, author: { id: context.user.id } },
  })
  if (!card) { throw new HttpError(403) }
  
  return context.entities.Card.update({
    where: { id: cardId },
    data: {
      pos: data.pos,
      list: { connect: { id: parseInt(data.listId) } }
    }
  })
}
