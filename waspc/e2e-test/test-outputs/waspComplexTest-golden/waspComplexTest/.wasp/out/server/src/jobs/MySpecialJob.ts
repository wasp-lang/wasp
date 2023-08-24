import prisma from '../dbClient.js'
import type { JSONValue } from '../_types/serialization.js'
import type { Job } from './core/job.js'
import { createJob } from './core/pgBoss/pgBossJob.js'
import { foo } from '../ext-src/jobs/bar.js'

const entities = {
};

export type MySpecialJob<Input extends object, Output extends JSONValue | void> = Job<Input, Output, typeof entities>["jobFn"]

export const MySpecialJob = createJob({
  jobName: "MySpecialJob",
  jobFn: foo,
  defaultJobOptions: {},
  jobSchedule: null,
  entities,
})
