import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

let someResource = undefined

export const getSomeResource = () => someResource

const setup = async () => {
  await new Promise(resolve => setTimeout(resolve, 2000))
  someResource = 'This resource is now set up.'
  console.log('Custom server setup done!')

  console.log('Kicking off Job...')
  // Or: const scheduledJob = await mySpecialJob.delay(10).submit({ something: "here" })
  const scheduledJob = await mySpecialJob.submit({ something: "here" })
  console.log("Job:", scheduledJob)
  console.log("scheduledJob.PgBoss.details()", await scheduledJob.PgBoss.details())
}

export default setup
