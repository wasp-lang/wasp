
import { Server } from 'socket.io'
import { EventsMap, DefaultEventsMap } from '@socket.io/component-emitter'

import { prisma } from 'wasp/server'
import { type AuthUser } from 'wasp/auth'

import { webSocketFn as webSocketFn_ext } from 'wasp/src/features/chat/webSocket'

// Public API
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
      User: typeof prisma.user,
      Task: typeof prisma.task,
      TaskVote: typeof prisma.taskVote,
      UppercaseTextRequest: typeof prisma.uppercaseTextRequest,
    }
  }
) => Promise<void> | void

// PUBLIC API
export interface WaspSocketData {
  user?: AuthUser
}

// PRIVATE API (framework)
export type ServerType = Parameters<WebSocketFn>[0]

// PRIVATE API (sdk)
export type ClientToServerEvents = Events[0]
// PRIVATE API (sdk)
export type ServerToClientEvents = Events[1]

type WebSocketFn = typeof webSocketFn_ext
type Events = ServerType extends Server<
  infer ClientToServerEvents,
  infer ServerToClientEvents
>
  ? [ClientToServerEvents, ServerToClientEvents]
  : [DefaultEventsMap, DefaultEventsMap]

