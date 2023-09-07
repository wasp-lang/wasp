import prisma from '../dbClient.js'
import type { JSONValue, JSONObject } from '../_types/serialization.js'
import { createJob, type JobFn } from './core/pgBoss/pgBossJob.js'
import { foo } from '../ext-src/jobs/bar.js'

const entities = {
};

export type MySpecialJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

export const MySpecialJob = createJob({
  jobName: "MySpecialJob",
  jobFn: foo,
  defaultJobOptions: {},
  jobSchedule: null,
  entities,
})
