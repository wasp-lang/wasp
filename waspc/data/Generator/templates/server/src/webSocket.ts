{{={= =}=}}

import http from 'http'
import { Server, Socket } from 'socket.io'
import { EventsMap, DefaultEventsMap } from '@socket.io/component-emitter'

import config from './config.js'
import prisma from './dbClient.js'

{=# isAuthEnabled =}
import { getUserFromToken } from './core/auth.js'
{=/ isAuthEnabled =}

{=& userWebSocketFn.importStatement =}

export type WebSocketDefinition<
  ClientToServerEvents extends EventsMap = DefaultEventsMap,
  ServerToClientEvents extends EventsMap = DefaultEventsMap,
  InterServerEvents extends EventsMap = DefaultEventsMap,
  SocketData extends WaspSocketData = WaspSocketData
> =
  ( io: Server<ClientToServerEvents, ServerToClientEvents, InterServerEvents, SocketData>,
    context: {
      entities: {
        {=# entities =}
        {= name =}: typeof prisma.{= prismaIdentifier =},
        {=/ entities =}
      }
    }
  ) => Promise<void> | void

export interface WaspSocketData {
  user?: any;
}

type WebSocketFn = typeof {= userWebSocketFn.importIdentifier =}
type ServerType = Parameters<WebSocketFn>[0]
type Events = ServerType extends Server<infer ClientToServerEvents, infer ServerToClientEvents>
  ? [ClientToServerEvents, ServerToClientEvents]
  : [DefaultEventsMap, DefaultEventsMap]

export type ClientToServerEvents = Events[0]
export type ServerToClientEvents = Events[1]

// Initializes the WebSocket server and invokes the user's WebSocket function.
export async function init(server: http.Server): Promise<void> {
  // TODO: In the future, we can consider allowing a clustering option.
  // Ref: https://github.com/wasp-lang/wasp/issues/1228
  const io: ServerType = new Server(server, {
    cors: {
      origin: config.frontendUrl,
    }
  })

  {=# isAuthEnabled =}
  io.use(addUserToSocketDataIfAuthenticated)
  {=/ isAuthEnabled =}

  const context = {
      entities: {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    }
  }

  await {= userWebSocketFn.importIdentifier =}(io, context)
}

{=# isAuthEnabled =}
async function addUserToSocketDataIfAuthenticated(socket: Socket, next: (err?: Error) => void) {
  const token = socket.handshake.auth.token
  if (token) {
    try {
      socket.data = { ...socket.data, user: await getUserFromToken(token) }
    } catch (err) { }
  }
  next()
}
{=/ isAuthEnabled =}
