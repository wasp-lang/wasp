import { v4 as uuidv4 } from 'uuid'

export function webSocketFn(io) {
  console.log('webSocketFn')

  io.on('connection', (socket) => {
    const username = socket.data?.user?.email || socket.data?.user?.username || 'unknown'
    console.log('a user connected: ', username)

    socket.on('chat message', (msg) => {
      io.emit('chat message', { id: uuidv4(), username, text: msg })
    })
  })
}
