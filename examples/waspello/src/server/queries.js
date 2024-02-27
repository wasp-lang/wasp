import { HttpError } from "wasp/server";

export const getListsAndCards = async (args, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.List.findMany({
    // We want to make sure user can get only his own info.
    where: { user: { id: context.user.id } },
    include: { cards: true }
  })
}
