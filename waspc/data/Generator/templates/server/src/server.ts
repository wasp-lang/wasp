{{={= =}=}}
import http from 'http'

import app from './app.js'
import config from './config.js'

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
import { ServerSetupFn, ServerSetupFnContext } from './types'
{=/ setupFn.isDefined =}

{=# isPgBossJobExecutorUsed =}
import { startPgBoss } from './jobs/core/pgBoss/pgBoss.js'
import './jobs/core/allJobs.js'
{=/ isPgBossJobExecutorUsed =}

{=# userWebSocketFn.isDefined =}
import { init as initWebSocket } from './webSocket/initialization.js'
{=/ userWebSocketFn.isDefined =}

const startServer = async () => {
  {=# isPgBossJobExecutorUsed =}
  await startPgBoss()
  {=/ isPgBossJobExecutorUsed =}

  const port = normalizePort(config.port)
  app.set('port', port)

  const server = http.createServer(app)

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

/**
 * Normalize a port into a number, string, or false.
 */
function normalizePort (val) {
  const port = parseInt(val, 10)
  if (isNaN(port)) return val // named pipe
  if (port >= 0) return port // port number
  return false
}
