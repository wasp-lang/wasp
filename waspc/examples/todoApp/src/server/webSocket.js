export function webSocketFn(io) {
  console.log('webSocketFn')

  let messageCount = 0

  io.on('connection', (socket) => {
    const username = socket.data?.user?.email || socket.data?.user?.username || 'unknown'
    console.log('a user connected: ', username)

    socket.on('chat message', (msg) => {
      io.emit('chat message', { id: messageCount++, text: `${username}: ${msg}` })
    })
  })
}
