import HttpError from '@wasp/core/HttpError.js'

export const createThought = async (args, context) => {
  if (!context.user) { throw new HttpError(403) }

  args.tagNames?.map(tagName => {
    if (!/^[a-z](\.?[a-z0-9])*$/.test(tagName)) {
      throw new HttpError(400, "Tag must contain only lowercase letters and dots.")
    }
  })
  return context.entities.Thought.create({
    data: {
      textMarkdown: args.textMarkdown,
      tags: {
        connectOrCreate: args.tagNames?.map(tagName => ({
          where: { name_userId: { name: tagName, userId: context.user.id } },
          create: { name: tagName, user: { connect: { id: context.user.id } } }
        }))
      },
      user: { connect: { id: context.user.id } }
    }
  })
}
