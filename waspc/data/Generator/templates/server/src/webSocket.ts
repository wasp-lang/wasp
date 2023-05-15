{{={= =}=}}

import http from 'http'
import { Server, Socket } from 'socket.io'
import { EventsMap, DefaultEventsMap } from '@socket.io/component-emitter'

import { getUserFromToken } from './core/auth.js'
import config from './config.js'
import { WaspSocketData } from './universal/types.js'

{=& webSocket.fn.importStatement =}

export type WebSocketDefinition<
  ClientToServerEvents extends EventsMap = DefaultEventsMap,
  ServerToClientEvents extends EventsMap = ClientToServerEvents,
  InterServerEvents extends EventsMap = DefaultEventsMap,
  SocketData extends WaspSocketData = WaspSocketData> =
  (io: Server<ClientToServerEvents, ServerToClientEvents, InterServerEvents, SocketData>) => Promise<void> | void

type ServerType = Parameters<typeof {= webSocket.fn.importIdentifier =}> [0]

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
      } catch (err) { }
    }
    next()
  })

  await {= webSocket.fn.importIdentifier =}(io)
}
