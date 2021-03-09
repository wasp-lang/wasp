import HttpError from '@wasp/core/HttpError.js'

export const createThought = async (args, context) => {
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
          where: { name: tagName },
          create: { name: tagName }
        }))
      }
    }
  })
}
