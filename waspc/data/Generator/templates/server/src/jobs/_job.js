{{={= =}=}}
import prisma from '../dbClient.js'
import { createJob } from './{= executorJobRelFP =}'
{=& jobPerformFnImportStatement =}

export const {= jobName =} = createJob({
  jobName: "{= jobName =}",
  jobFn: {= jobPerformFnName =},
  defaultJobOptions: {=& jobPerformOptions =},
  jobSchedule: {=& jobSchedule =},
  entities: {
    {=# entities =}
    {= name =}: prisma.{= prismaIdentifier =},
    {=/ entities =}
  },
})
