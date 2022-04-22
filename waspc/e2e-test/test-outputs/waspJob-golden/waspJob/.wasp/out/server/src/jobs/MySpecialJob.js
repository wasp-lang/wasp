import { createJob } from './pgBossJob.js'
import { foo } from './../ext-src/jobs/bar.js'

export const MySpecialJob = await createJob("MySpecialJob", foo, {})
