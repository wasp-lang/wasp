{{={= =}=}}

import http from 'http'
import { Server, Socket } from 'socket.io'

import { getUserFromToken } from './core/auth.js'
import config from './config.js'

{=& webSocket.fn.importStatement =}

export type WebSocketDefinition<T extends Server = Server> = (io: T) => Promise<void> | void

type ServerType = Parameters<typeof {= webSocket.fn.importIdentifier =}>[0]

export async function init(server: http.Server): Promise<void> {
  const io: ServerType = new Server(server, {
    cors: {
      origin: config.frontendUrl,
    }
  })

  io.use(async (socket: Socket, next: (err?: Error) => void) => {
    const token = socket.handshake.auth.token
    if (token) {
      try {
        socket.data = { ...socket.data, user: await getUserFromToken(token) }
      } catch (err) {}
    }
    next()
  })

  await {= webSocket.fn.importIdentifier =}(io)
}
