import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'
import { sayHi } from '../shared/util.js'

let someResource = undefined

export const getSomeResource = () => someResource

const setup = async (app) => {
  addCustomRoute(app)
  sayHi()
  await new Promise(resolve => setTimeout(resolve, 2000))
  someResource = 'This resource is now set up.'
  console.log('Custom server setup done!')

  console.log('Kicking off Job...')
  // Or: const submittedJob = await mySpecialJob.delay(10).submit({ something: "here" })
  const submittedJob = await mySpecialJob.submit({ something: "here" })
  console.log(submittedJob.jobId, submittedJob.jobName, submittedJob.executorName)
  console.log("submittedJob.pgBoss.details()", await submittedJob.pgBoss.details())
}

function addCustomRoute(app) {
  app.get('/customRoute', (_req, res) => {
    res.send('I am a custom route')
  })
}

export default setup
