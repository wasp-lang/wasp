import prisma from '../dbClient.js'
import { JSONValue } from '../_types/serialization.js'
import { createJob } from './core/pgBoss/pgBossJob.js'
import { foo } from '../ext-src/jobs/bar.js'

const entities = {
};

export type MySpecialJob<Input extends JSONValue, Output extends JSONValue> = (args: Input, context: {
  entities: typeof entities
}) => Promise<Output> | void

export const MySpecialJob = createJob({
  jobName: "MySpecialJob",
  jobFn: foo,
  defaultJobOptions: {},
  jobSchedule: null,
  entities,
})
