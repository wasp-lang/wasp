{{={= =}=}}
import { jobFactory } from './{= jobFactoryName =}.js'
{=& jobPerformFnImportStatement =}

export const {= jobName =} = jobFactory("{= jobName =}", {= jobPerformFnName =})
