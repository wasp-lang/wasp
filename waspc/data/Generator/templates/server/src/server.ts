{{={= =}=}}
import http from 'http'
import express from 'express'

import app, { handleHttpError } from './app.js'
import { config } from 'wasp/server'
import { globalMiddlewareConfigForExpress } from './middleware/index.js'
{=# areThereAnyCustomApiRoutes =}
import { rootRouter as rootApis } from './routes/apis/index.js'
{=/ areThereAnyCustomApiRoutes =}

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
import { ServerSetupFn } from 'wasp/server'
import { ServerSetupFnContext } from 'wasp/server/types'
{=/ setupFn.isDefined =}

{=# isPgBossJobExecutorUsed =}
import { startPgBoss } from 'wasp/server/jobs/core/pgBoss'
import './jobs/core/allJobs.js'
{=/ isPgBossJobExecutorUsed =}

{=# userWebSocketFn.isDefined =}
import { init as initWebSocket } from './webSocket/initialization.js'
{=/ userWebSocketFn.isDefined =}

{=# isDevelopment =}
import { makeWrongPortPage } from './views/wrong-port.js'
{=/ isDevelopment =}

// The whole app (Wasp's routes, user apis and anything added in `setupFn`) lives under the
// server base path, so it is mounted there on a root app that owns everything else.
const rootApp = express()
// Helmet only runs on routes, so both apps disable the header themselves.
rootApp.disable('x-powered-by')
rootApp.use('{=& serverBasePath =}', app)
{=# areThereAnyCustomApiRoutes =}
// Apis that set `ignoreServerBasePath` are matched after everything under the base path,
// so they can never shadow Wasp's routes, even when the base path is `/`.
rootApp.use(rootApis)
{=/ areThereAnyCustomApiRoutes =}

// The server root stays at the origin root, outside the server base path.
{=# isDevelopment =}
rootApp.get('/', globalMiddlewareConfigForExpress(), sendWrongPortPage)
{=/ isDevelopment =}
{=^ isDevelopment =}
// In production the server root has nothing to serve, so it answers with an empty 200.
rootApp.get('/', globalMiddlewareConfigForExpress(), function (_req, res) {
  res.status(200).send()
})
{=/ isDevelopment =}

// Errors from under the base path were already handled inside `app`; this serves `rootApis` and the root page.
rootApp.use(handleHttpError)

const startServer = async () => {
  {=# isPgBossJobExecutorUsed =}
  await startPgBoss()
  {=/ isPgBossJobExecutorUsed =}

  const port = normalizePort(config.port)
  app.set('port', port)

  const server = http.createServer(rootApp)

  {=# setupFn.isDefined =}
  const serverSetupFnContext: ServerSetupFnContext = { app, server }
  await ({= setupFn.importIdentifier =} as ServerSetupFn)(serverSetupFnContext)
  {=/ setupFn.isDefined =}

  {=# userWebSocketFn.isDefined =}
  await initWebSocket(server)
  {=/ userWebSocketFn.isDefined =}

  server.listen(port)

  server.on('error', (error: NodeJS.ErrnoException) => {
    if (error.syscall !== 'listen') throw error
    const bind = typeof port === 'string' ? 'Pipe ' + port : 'Port ' + port
    // handle specific listen errors with friendly messages
    switch (error.code) {
    case 'EACCES':
      console.error(bind + ' requires elevated privileges')
      process.exit(1)
    case 'EADDRINUSE':
      console.error(bind + ' is already in use')
      process.exit(1)
    default:
      throw error
    }
  })

  server.on('listening', () => {
    const addr = server.address()
    const bind = typeof addr === 'string' ? 'pipe ' + addr : 'port ' + addr.port
    console.log('Server listening on ' + bind)
  })
}

startServer().catch(e => console.error(e))

{=# isDevelopment =}
function sendWrongPortPage(_req, res) {
  const wrongPortPage = makeWrongPortPage({
    appName: "{= appName =}",
    frontendUrl: config.frontendUrl,
  })
  res.status(200).type('html').send(wrongPortPage)
}
{=/ isDevelopment =}

/**
 * Normalize a port into a number, string, or false.
 */
function normalizePort (val) {
  const port = parseInt(val, 10)
  if (isNaN(port)) return val // named pipe
  if (port >= 0) return port // port number
  return false
}
