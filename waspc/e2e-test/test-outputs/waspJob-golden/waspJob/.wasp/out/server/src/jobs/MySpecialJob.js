import { createJob, executorSetup } from './pgBossJob.js'
import { foo } from './../ext-src/jobs/bar.js'

await executorSetup({ jobName: "MySpecialJob", jobFn: foo })

export const MySpecialJob = createJob({ jobName: "MySpecialJob", jobFn: foo, defaultJobOptions: {} })
