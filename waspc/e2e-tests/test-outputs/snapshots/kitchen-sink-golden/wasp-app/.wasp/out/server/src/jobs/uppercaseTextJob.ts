import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { uppercaseTextJob } from "../../../../../src/features/jobs/uppercaseText"
import { uppercaseTextJob as _waspJobDefinition } from "wasp/server/jobs"

await registerJob({
  job: _waspJobDefinition,
  jobFn: uppercaseTextJob,
})
