import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'
import { sayHi } from '../shared/util.js'
import { ServerSetupFn, Application } from '@wasp/types'
import { MiddlewareConfigFn } from '@wasp/middleware'

let someResource = undefined

export const getSomeResource = () => someResource

const setup: ServerSetupFn = async ({ app }) => {
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

function addCustomRoute(app: Application) {
  app.get('/customRoute', (_req, res) => {
    res.set('Access-Control-Allow-Origin', 'example-cors-override.com')
    res.removeHeader('X-Frame-Options')
    res.send('I am a custom route')
  })
}

export const serverMiddlewareFn: MiddlewareConfigFn = (middleware) => {
  middleware.set('custom.global',
    (req, _res, next) => {
      console.log(`serverMiddlewareFn: custom global middleware (path: ${req.path})`)
      next()
    }
  )
  return middleware
}

export default setup
