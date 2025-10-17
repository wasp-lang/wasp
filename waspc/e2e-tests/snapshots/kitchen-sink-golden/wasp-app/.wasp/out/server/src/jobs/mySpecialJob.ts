import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { foo } from '../../../../../src/features/jobs/bar'
import { mySpecialJob as _waspJobDefinition } from 'wasp/server/jobs'

registerJob({
  job: _waspJobDefinition,
  jobFn: foo,
})
