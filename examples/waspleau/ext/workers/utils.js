export function upsertMetric(context) {
  return ({ name, value } = {}) => {
    return context.entities.Metric.upsert({
      where: { name },
      update: { name, value: String(value) },
      create: { name, value: String(value) }
    })
  }
}
