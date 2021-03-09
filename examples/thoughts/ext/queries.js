export const getThoughts = async (args, context) => {
  return context.entities.Thought.findMany({
    orderBy: [{ createdAt: 'desc' }],
    include: { tags: true },
    where: {
      tags: { some: { name: args.tagName || undefined } }
    }
  })
}

export const getTags = async (args, context) => {
  return context.entities.Tag.findMany({
    orderBy: [{ name: 'asc' }],
  })
}
