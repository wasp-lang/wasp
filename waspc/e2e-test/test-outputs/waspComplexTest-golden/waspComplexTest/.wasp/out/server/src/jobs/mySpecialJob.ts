import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { foo as __userDefinedFoo } from '../../../../../src/server/jobs/bar.js'
import { mySpecialJob as __userDefinedMySpecialJob } from 'wasp/server/jobs'

registerJob({
  job: __userDefinedMySpecialJob,
  jobFn: __userDefinedFoo,
})
