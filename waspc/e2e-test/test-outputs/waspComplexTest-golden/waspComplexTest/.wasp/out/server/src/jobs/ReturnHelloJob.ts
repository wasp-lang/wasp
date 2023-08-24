import prisma from '../dbClient.js'
import type { JSONValue } from '../_types/serialization.js'
import type { Job } from './core/job.js'
import { createJob } from './core/pgBoss/pgBossJob.js'
import { returnHello } from '../ext-src/jobs/returnHello.js'

const entities = {
  User: prisma.user,
};

export type ReturnHelloJob<Input extends object, Output extends JSONValue | void> = Job<Input, Output, typeof entities>["jobFn"]

export const ReturnHelloJob = createJob({
  jobName: "ReturnHelloJob",
  jobFn: returnHello,
  defaultJobOptions: {},
  jobSchedule: null,
  entities,
})
