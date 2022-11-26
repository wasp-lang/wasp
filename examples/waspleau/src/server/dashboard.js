export const refreshDashboardData = async (_args, context) => {
  return context.entities.Datum.findMany({
    orderBy: [
      {
        name: 'asc',
      },
    ],
  })
}
