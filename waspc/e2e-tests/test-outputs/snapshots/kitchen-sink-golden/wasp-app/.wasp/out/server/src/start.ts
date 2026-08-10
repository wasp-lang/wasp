import http from 'http'

import app from './app.js'
import { config, prisma } from 'wasp/server'

import { serverSetup } from '../../../../src/serverSetup'
import { ServerSetupFn } from 'wasp/server'
import { ServerSetupFnContext } from 'wasp/server/types'

import { startPgBoss, stopPgBoss } from 'wasp/server/jobs/core/pgBoss'
import './jobs/core/allJobs.js'

import { init as initWebSocket } from './webSocket/initialization.js'

export type ServerHandle = {
  /**
   * Stops the server and disposes of everything it started.
   */
  close: () => Promise<void>
}

/**
 * Starts the server and returns a handle for stopping it.
 *
 * It rejects if the server fails to start (e.g. if its port is already taken),
 * so the caller can decide what to do about it.
 */
export async function startServer(): Promise<ServerHandle> {
  await startPgBoss()

  const port = normalizePort(config.port)
  app.set('port', port)

  const server = http.createServer(app)

  const serverSetupFnContext: ServerSetupFnContext = { app, server }
  await (serverSetup as ServerSetupFn)(serverSetupFnContext)

  const io = await initWebSocket(server)

  await listen(server, port)

  const addr = server.address()
  const bind = typeof addr === 'string' ? 'pipe ' + addr : 'port ' + addr.port
  console.log('Server listening on ' + bind)

  return {
    async close() {
      await closeHttpServer(server)
      io.close()
      await prisma.$disconnect()
      await stopPgBoss()
    },
  }
}

/**
 * Starts listening on the given port, resolving once the server is listening
 * and rejecting if it can't listen.
 */
function listen(server: http.Server, port): Promise<void> {
  return new Promise((resolve, reject) => {
    const onError = (error: NodeJS.ErrnoException) => {
      server.off('listening', onListening)
      reject(makeListenErrorFriendlier(error, port))
    }
    const onListening = () => {
      server.off('error', onError)
      resolve()
    }
    server.once('error', onError)
    server.once('listening', onListening)
    server.listen(port)
  })
}

/**
 * Replaces the most common listen errors with friendlier messages, keeping the
 * original error as the cause.
 */
function makeListenErrorFriendlier(error: NodeJS.ErrnoException, port): Error {
  if (error.syscall !== 'listen') return error
  const bind = typeof port === 'string' ? 'Pipe ' + port : 'Port ' + port
  switch (error.code) {
  case 'EACCES':
    return new Error(bind + ' requires elevated privileges', { cause: error })
  case 'EADDRINUSE':
    return new Error(bind + ' is already in use', { cause: error })
  default:
    return error
  }
}

function closeHttpServer(server: http.Server): Promise<void> {
  return new Promise((resolve, reject) => {
    server.close((error) => error ? reject(error) : resolve())
    // `close` stops the server from accepting new connections, but it only
    // completes once all the existing connections are done, so we close them.
    server.closeAllConnections()
  })
}

/**
 * Normalize a port into a number, string, or false.
 */
function normalizePort (val) {
  const port = parseInt(val, 10)
  if (isNaN(port)) return val // named pipe
  if (port >= 0) return port // port number
  return false
}
