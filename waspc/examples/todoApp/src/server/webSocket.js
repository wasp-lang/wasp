export function webSocketFn(io) {
  console.log('webSocketFn')
  io.on('connection', (socket) => {
    console.log('a user connected')
    socket.on('msg', (msg) => { console.log('msg: ', msg) })
    socket.on('disconnect', () => {
      console.log('user disconnected')
    })
  })
}
