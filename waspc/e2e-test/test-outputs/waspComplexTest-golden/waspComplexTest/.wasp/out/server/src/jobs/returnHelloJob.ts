import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { returnHello as __userDefinedReturnHello } from '../../../../../src/server/jobs/returnHello.js'
import { returnHelloJob as __userDefinedReturnHelloJob } from 'wasp/server/jobs'

registerJob({
  job: __userDefinedReturnHelloJob,
  jobFn: __userDefinedReturnHello,
})
