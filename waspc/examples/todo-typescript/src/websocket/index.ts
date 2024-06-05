import { WebSocketDefinition } from 'wasp/server/webSocket'
import { getFirstProviderUserId } from 'wasp/auth'

export const webSocketFn: WebSocketDefinition<
  ClientToServerEvents,
  ServerToClientEvents,
  InterServerEvents
> = (io, context) => {
  io.on('connection', (socket) => {
    const username = socket.data.user?.getFirstProviderUserId() ?? 'Unknown'
    console.log('a user connected: ', username)

    socket.on('chatMessage', async (msg) => {
      console.log('message: ', msg)
      io.emit('chatMessage', { id: 'random', username, text: msg })
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
