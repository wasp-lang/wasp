{{={= =}=}}
import { createJob } from './core/{= executorJobFilename =}'
{=& jobPerformFnImportStatement =}

export const {= jobName =} = await createJob({ jobName: "{= jobName =}", jobFn: {= jobPerformFnName =}, defaultJobOptions: {=& jobPerformOptions =} })

// TODO: should we try to put pg boss under its own (with an index.js perhaps?)
