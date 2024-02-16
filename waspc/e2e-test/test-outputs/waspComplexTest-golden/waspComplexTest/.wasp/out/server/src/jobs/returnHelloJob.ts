import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { returnHello } from '../../../../../src/server/jobs/returnHello.js'
import { returnHelloJob as _waspJobDefinition } from 'wasp/server/jobs'

registerJob({
  job: _waspJobDefinition,
  jobFn: returnHello,
})
