import { jobFactory } from './PassthroughJobFactory.js'
import { foo } from './../ext-src/jobs/bar.js'

export const MySpecialJob = jobFactory(foo)
