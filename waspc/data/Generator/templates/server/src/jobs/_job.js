{{={= =}=}}
import { createJob } from './{= executorJobRelFP =}'
{=& jobPerformFnImportStatement =}

export const {= jobName =} = await createJob({ jobName: "{= jobName =}", jobFn: {= jobPerformFnName =}, defaultJobOptions: {=& jobPerformOptions =} })
