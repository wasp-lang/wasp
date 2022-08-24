export const saveExcuse = async (excuse, context) => {
  return context.entities.Excuse.create({
    data: { text: excuse.text }
  })
}