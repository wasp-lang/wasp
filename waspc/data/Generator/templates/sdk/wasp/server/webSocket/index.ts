{{={= =}=}}

import { Server } from 'socket.io'
import { EventsMap, DefaultEventsMap } from '@socket.io/component-emitter'

import { prisma } from 'wasp/server'
import type { FromRegistry } from 'wasp/types'
{=# isAuthEnabled =}
import { type AuthUser } from 'wasp/auth'
{=/ isAuthEnabled =}


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
      {=# allEntities =}
      {= name =}: typeof prisma.{= prismaIdentifier =},
      {=/ allEntities =}
    }
  }
) => Promise<void> | void

// PUBLIC API
export interface WaspSocketData {
  {=# isAuthEnabled =}
  user?: AuthUser
  {=/ isAuthEnabled =}
}

// PRIVATE API (framework)
export type ServerType = Parameters<WebSocketFn>[0]

// PRIVATE API (sdk)
export type ClientToServerEvents = Events[0]
// PRIVATE API (sdk)
export type ServerToClientEvents = Events[1]

type WebSocketFn = FromRegistry<'webSocketFn', WebSocketDefinition>;
type Events = ServerType extends Server<
  infer ClientToServerEvents,
  infer ServerToClientEvents
>
  ? [ClientToServerEvents, ServerToClientEvents]
  : [DefaultEventsMap, DefaultEventsMap]

