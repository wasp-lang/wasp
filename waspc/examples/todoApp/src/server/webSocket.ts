import { v4 as uuidv4 } from 'uuid'
import { WebSocketDefinition } from '@wasp/webSocket'

export const webSocketFn: WebSocketDefinition<
  ClientToServerEvents,
  ServerToClientEvents,
  InterServerEvents
> = (io, context) => {
  io.on('connection', (socket) => {
    const username =
      socket.data.user?.email || socket.data.user?.username || 'unknown'
    console.log('a user connected: ', username)

    socket.on('chatMessage', async (msg) => {
      console.log('message: ', msg)
      io.emit('chatMessage', { id: uuidv4(), username, text: msg })
    })
  })
}

interface ServerToClientEvents {
  chatMessage: (msg: { id: string; username: string; text: string }) => void
}
interface ClientToServerEvents {
  chatMessage: (msg: string) => void
}
interface InterServerEvents {}
