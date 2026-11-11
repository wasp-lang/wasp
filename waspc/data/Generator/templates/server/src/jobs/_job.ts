{{={= =}=}}
import { registerJob } from '{= jobExecutorImportPath =}'
{=& jobPerformFn.importStatement =}
{=& jobDefinition.importStatement =}

await registerJob({
  job: {= jobDefinition.importIdentifier =},
  jobFn: {= jobPerformFn.importIdentifier =},
})
