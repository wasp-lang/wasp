export const refreshDashboardData = async (_args, context) => {
  return context.entities.Metric.findMany({
    orderBy: [
      {
        name: 'asc',
      },
    ],
  })
}
