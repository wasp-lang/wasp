import { createJob } from './core/pgBoss/pgBossJob.js'
import { foo } from './../ext-src/jobs/bar.js'

export const MySpecialJob = createJob({
  jobName: "MySpecialJob",
  jobFn: foo,
  defaultJobOptions: {},
  jobSchedule: null
})
