export function webSocketFn(io) {
  console.log('webSocketFn');
  io.on('connection', (socket) => {
    console.log('a user connected');
    socket.on('disconnect', () => {
      console.log('user disconnected');
    });
  });
}
