import prisma from '../dbClient.js'
import type { JSONValue, JSONObject } from '../_types/serialization.js'
import { createJob, type JobFn } from './core/pgBoss/pgBossJob.js'
import { returnHello } from '../ext-src/jobs/returnHello.js'

const entities = {
  User: prisma.user,
};

export type ReturnHelloJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

export const ReturnHelloJob = createJob({
  jobName: "ReturnHelloJob",
  jobFn: returnHello,
  defaultJobOptions: {},
  jobSchedule: null,
  entities,
})
