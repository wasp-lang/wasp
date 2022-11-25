export function upsertData(context) {
  return ({ name, value } = {}) => {
    return context.entities.Datum.upsert({
      where: { name },
      update: { name, value: String(value) },
      create: { name, value: String(value) }
    })
  }
}
