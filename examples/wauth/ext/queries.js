export const getTasks = async (args, context) => {
  return context.entities.Task.findMany({})
}
