
import http from 'http'
import { Server, Socket } from 'socket.io'
import type { ServerType } from 'wasp/server/webSocket'

import { config, prisma } from 'wasp/server'

import { getSessionAndUserFromSessionId } from 'wasp/auth/session'
import { makeAuthUserIfPossible } from 'wasp/auth/user'

import { webSocketFn } from '../../../../../src/features/chat/webSocket'

// Initializes the WebSocket server and invokes the user's WebSocket function.
export async function init(server: http.Server): Promise<void> {
  // TODO: In the future, we can consider allowing a clustering option.
  // Ref: https://github.com/wasp-lang/wasp/issues/1228

  // TODO: Uncomment the type annotation once we make sure that the types between different packages are aligned.
  // Ref: https://github.com/wasp-lang/wasp/issues/2726
  const io /* : ServerType */ = new Server(server, {
    cors: {
      origin: config.frontendUrl,
    }
  })

  io.use(addUserToSocketDataIfAuthenticated)

  const context = {
    entities: {
      User: prisma.user,
      Task: prisma.task,
      TaskVote: prisma.taskVote,
      UppercaseTextRequest: prisma.uppercaseTextRequest,
    }
  }

  await (webSocketFn as any)(io, context)
}

async function addUserToSocketDataIfAuthenticated(socket: Socket, next: (err?: Error) => void) {
  const sessionId = socket.handshake.auth.sessionId
  if (sessionId) {
    try {
      const sessionAndUser = await getSessionAndUserFromSessionId(sessionId)
      const user = sessionAndUser ? makeAuthUserIfPossible(sessionAndUser.user) : null
      socket.data = {
        ...socket.data,
        user,
      }
    } catch (err) { }
  }
  next()
}
