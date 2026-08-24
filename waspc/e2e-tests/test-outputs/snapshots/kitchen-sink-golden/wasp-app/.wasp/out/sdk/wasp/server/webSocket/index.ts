
import { Server } from 'socket.io'
import { EventsMap, DefaultEventsMap } from '@socket.io/component-emitter'

import { prisma } from '../index.js'
import type { FromRegister } from '../../types/register'
import { type AuthUser } from '../../auth/user.js'


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
export type ServerType = Parameters<RegisteredWebSocketFn>[0]

// PRIVATE API (sdk)
export type ClientToServerEvents = Events[0]
// PRIVATE API (sdk)
export type ServerToClientEvents = Events[1]

type RegisteredWebSocketFn = FromRegister<'webSocketFn', WebSocketDefinition>;
type Events = ServerType extends Server<
  infer ClientToServerEvents,
  infer ServerToClientEvents
>
  ? [ClientToServerEvents, ServerToClientEvents]
  : [DefaultEventsMap, DefaultEventsMap]
