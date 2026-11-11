import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { mySpecialJob } from "../../../../../src/features/jobs/bar"
import { mySpecialJob as _waspJobDefinition } from "wasp/server/jobs"

await registerJob({
  job: _waspJobDefinition,
  jobFn: mySpecialJob,
})
