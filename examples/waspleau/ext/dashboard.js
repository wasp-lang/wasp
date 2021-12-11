import { getDashboardData } from './serverSetup.js'

export const refreshDashboardData = async (_args, _context) => {
  return getDashboardData()
}
