{{={= =}=}}

import http from 'http'
import { Server, Socket } from 'socket.io'
import type { ServerType } from 'wasp/server/webSocket'

import { config, prisma } from 'wasp/server'

{=# isAuthEnabled =}
import { getSessionAndUserFromSessionId } from 'wasp/auth/session'
import { makeAuthUserIfPossible } from 'wasp/auth/user'
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
      const sessionAndUser =  await getSessionAndUserFromSessionId(sessionId)
      const user = sessionAndUser ? makeAuthUserIfPossible(sessionAndUser.user) : null
      socket.data = {
        ...socket.data,
        user,
      }
    } catch (err) { }
  }
  next()
}
{=/ isAuthEnabled =}
