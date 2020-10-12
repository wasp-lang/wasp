export const createTask = async ({ description }, context) => {
  return context.entities.Task.create({
    data: { description }
  })
}

export const updateTask = async (args, context) => {
  return context.entities.Task.update({
    where: { id: args.taskId },
    data: args.data
  })
}
