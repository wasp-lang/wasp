{{={= =}=}}

import { Server } from 'socket.io'

import { getUserFromToken } from './core/auth.js'
import config from './config.js'

{=& webSocket.fn.importStatement =}

export async function init(server) {
  const io = new Server(server, {
    cors: {
      origin: config.frontendUrl,
    }
  })

  io.use(async (socket, next) => {
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
