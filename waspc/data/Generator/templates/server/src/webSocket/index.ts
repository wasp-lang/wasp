{{={= =}=}}

import { Server } from 'socket.io'
import { EventsMap, DefaultEventsMap } from '@socket.io/component-emitter'

import prisma from '../dbClient.js'

{=& userWebSocketFn.importStatement =}

export type WebSocketDefinition<
  ClientToServerEvents extends EventsMap = DefaultEventsMap,
  ServerToClientEvents extends EventsMap = DefaultEventsMap,
  InterServerEvents extends EventsMap = DefaultEventsMap,
  SocketData extends WaspSocketData = WaspSocketData
> = (
  io: Server<
    ClientToServerEvents,
    ServerToClientEvents,
    InterServerEvents,
    SocketData
  >,
  context: {
    entities: {
      {=# allEntities =}
      {= name =}: typeof prisma.{= prismaIdentifier =},
      {=/ allEntities =}
    }
  }
) => Promise<void> | void

export interface WaspSocketData {
  user?: any
}

export type ServerType = Parameters<WebSocketFn>[0]

export type ClientToServerEvents = Events[0]
export type ServerToClientEvents = Events[1]

type WebSocketFn = typeof {= userWebSocketFn.importIdentifier =}
type Events = ServerType extends Server<
  infer ClientToServerEvents,
  infer ServerToClientEvents
>
  ? [ClientToServerEvents, ServerToClientEvents]
  : [DefaultEventsMap, DefaultEventsMap]

