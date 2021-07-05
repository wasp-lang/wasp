import HttpError from '@wasp/core/HttpError.js'

export const getCards = async (args, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.Card.findMany({
    where: {
      list: { id: args.listId },
      // We want to make sure user can get only his own cards.
      author: { id: context.user.id }
    }
  })
}

export const getListsAndCards = async (args, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.List.findMany({
    where: { user: { id: context.user.id } },
    include: { cards: true }
  })
}
