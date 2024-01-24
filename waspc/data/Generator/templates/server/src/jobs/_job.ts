{{={= =}=}}
import { createJob } from './{= jobExecutorRelativePath =}'
{=& jobEntitiesImportStatement =}
{=& jobPerformFnImportStatement =}

export const {= jobName =} = createJob({
  jobName: "{= jobName =}",
  jobFn: {= jobPerformFnName =},
  defaultJobOptions: {=& jobPerformOptions =},
  jobSchedule: {=& jobSchedule =},
  {= jobEntitiesIdentifier =},
})
