import { createJob } from './core/pgBoss/pgBossJob.js'
import { foo } from './../ext-src/jobs/bar.js'

export const MySpecialJob = await createJob({ jobName: "MySpecialJob", jobFn: foo, defaultJobOptions: {} })
