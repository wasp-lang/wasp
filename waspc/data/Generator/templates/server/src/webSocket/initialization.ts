{{={= =}=}}

import http from 'http'
import { Server, Socket } from 'socket.io'
import type { ServerType } from './index.js'

import config from '../config.js'
import prisma from 'wasp/server/dbClient'

{=# isAuthEnabled =}
import { getSessionAndUserFromSessionId } from '../auth/session.js'
{=/ isAuthEnabled =}

{=& userWebSocketFn.importStatement =}

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
      {=# allEntities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ allEntities =}
    }
  }

  await ({= userWebSocketFn.importIdentifier =} as any)(io, context)
}

{=# isAuthEnabled =}
async function addUserToSocketDataIfAuthenticated(socket: Socket, next: (err?: Error) => void) {
  const sessionId = socket.handshake.auth.sessionId
  if (sessionId) {
    try {
      const { user } =  await getSessionAndUserFromSessionId(sessionId)
      socket.data = { ...socket.data, user }
    } catch (err) { }
  }
  next()
}
{=/ isAuthEnabled =}
