import HttpError from '@wasp/core/HttpError.js'

export const getThoughts = async (args, context) => {
  if (!context.user) { throw new HttpError(403) }

  return context.entities.Thought.findMany({
    orderBy: [{ createdAt: 'desc' }],
    include: { tags: true },
    where: {
      user: { id: context.user.id },
      tags: { some: { name: args.tagName || undefined } }
    }
  })
}

export const getTags = async (args, context) => {
  if (!context.user) { throw new HttpError(403) }

  return context.entities.Tag.findMany({
    orderBy: [{ name: 'asc' }],
    where: {
      user: { id: context.user.id }
    }
  })
}
