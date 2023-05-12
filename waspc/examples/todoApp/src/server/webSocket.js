import { getUserFromToken } from '@wasp/core/auth.js'

export function webSocketFn(io) {
  console.log('webSocketFn')

  // TODO: Pull out of this place into Wasp
  io.use(async (socket, next) => {
    const token = socket.handshake.auth.token
    if (token) {
      try {
        socket.context = { user: await getUserFromToken(token) }
      } catch (err) {}
    }
    console.log(`token: ${token}`)
    next()
  })

  io.on('connection', (socket) => {
    console.log('a user connected: ', socket.context?.user || 'no user')

    socket.on('ping', (msg) => {
      console.log('ping: ', msg)
      io.emit('pong', 'hello from webSocket.js')
    })

    socket.on('disconnect', () => {
      console.log('user disconnected')
    })
  })
}
