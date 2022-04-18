import { jobFactory } from './PgBossJobFactory.js'
import { foo } from './../ext-src/jobs/bar.js'

export const MySpecialJob = await jobFactory("MySpecialJob", foo, {})
