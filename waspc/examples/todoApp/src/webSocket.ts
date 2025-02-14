import { type WebSocketDefinition } from 'wasp/server/webSocket'
import { v4 as uuidv4 } from 'uuid'

export const webSocketFn: WebSocketDefinition<
  ClientToServerEvents,
  ServerToClientEvents,
  InterServerEvents
> = (io, context) => {

  const apiNamespace = io.of('/api');
  apiNamespace.on('connection', (socket) => {
    const username = socket.data.user?.getFirstProviderUserId() ?? 'Unknown'
    console.log('a user connected: ', username)

    socket.on('chatMessage', async (msg) => {
      console.log('message: ', msg)
      apiNamespace.emit('chatMessage', { id: uuidv4(), username, text: msg })
    })
  })

  // io.on('connection', (socket) => {
  //   const username = socket.data.user?.getFirstProviderUserId() ?? 'Unknown'
  //   console.log('a user connected: ', username)

  //   socket.on('chatMessage', async (msg) => {
  //     console.log('message: ', msg)
  //     io.emit('chatMessage', { id: uuidv4(), username, text: msg })
  //   })
  // })

}

interface ServerToClientEvents {
  chatMessage: (msg: { id: string; username: string; text: string }) => void
}
interface ClientToServerEvents {
  chatMessage: (msg: string) => void
}
interface InterServerEvents {}
