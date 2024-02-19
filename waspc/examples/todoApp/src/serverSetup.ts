import { type Application } from "express";
import { mySpecialJob } from "wasp/server/jobs";
import { config, type MiddlewareConfigFn, type ServerSetupFn } from "wasp/server";
import { sayHi } from './util.js'
import cors from 'cors'

let someResource: string | undefined = undefined

export const getSomeResource = () => someResource

const setup: ServerSetupFn = async ({ app }) => {
  addCustomRoute(app)

  sayHi()
  await new Promise((resolve) => setTimeout(resolve, 2000))
  someResource = 'This resource is now set up.'
  console.log('Custom server setup done!')

  console.log('Kicking off Job...')
  // Or: const submittedJob = await mySpecialJob.delay(10).submit({ something: "here" })
  const submittedJob = await mySpecialJob.submit({ something: 'here' })
  console.log(
    submittedJob.jobId,
    submittedJob.job.jobName,
    submittedJob.job.executorName
  )
  console.log(
    'submittedJob.pgBoss.details()',
    await submittedJob.pgBoss.details()
  )
}

function addCustomRoute(app: Application) {
  app.get('/customRoute', (_req, res) => {
    res.set('Access-Control-Allow-Origin', 'example-cors-override.com')
    res.removeHeader('X-Frame-Options')
    res.send('I am a custom route')
  })
}

export const serverMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  // Example of adding an extra domain to CORS.
  middlewareConfig.set(
    'cors',
    cors({ origin: [config.frontendUrl, 'http://127.0.0.1:3000'] })
  )
  return middlewareConfig
}

export default setup
