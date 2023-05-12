export function webSocketFn(io) {
  console.log('webSocketFn')
  io.on('connection', (socket) => {
    console.log('a user connected')

    socket.on('ping', (msg) => {
      console.log('ping: ', msg)
      io.emit('pong', 'hello from webSocket.js')
    })

    socket.on('disconnect', () => {
      console.log('user disconnected')
    })
  })
}
