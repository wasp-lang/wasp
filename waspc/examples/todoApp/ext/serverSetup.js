import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

let someResource = undefined

export const getSomeResource = () => someResource

const setup = async () => {
  await new Promise(resolve => setTimeout(resolve, 2000))
  someResource = 'This resource is now set up.'
  console.log('Custom server setup done!')

  console.log('Kicking off Job...')
  // Or: const submittedJob = await mySpecialJob.delay(10).submit({ something: "here" })
  const submittedJob = await mySpecialJob.submit({ something: "here" })
  console.log("Job:", submittedJob)
  console.log("submittedJob.pgBoss.details()", await submittedJob.pgBoss.details())
}

export default setup
