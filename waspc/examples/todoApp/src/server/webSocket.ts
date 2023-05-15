import { v4 as uuidv4 } from 'uuid'
import { Server } from 'socket.io'

import { WebSocketDefinition } from '@wasp/webSocket'

interface ServerToClientEvents {
  chatMessage: (res: {id: string, username: string, text: string}) => void;
}

interface ClientToServerEvents {
  chatMessage: (msg: string) => void;
}

interface InterServerEvents {
}

// TODO: Pull this out into Wasp
interface SocketData {
  user?: any;
}

type myServerType = Server<
  ClientToServerEvents,
  ServerToClientEvents,
  InterServerEvents,
  SocketData
>

export const webSocketFn: WebSocketDefinition<myServerType> = (io) => {
  console.log('webSocketFn')

  io.on('connection', (socket) => {
    const username = socket.data?.user?.email || socket.data?.user?.username || 'unknown'
    console.log('a user connected: ', username)

    socket.on('chatMessage', (msg) => {
      io.emit('chatMessage', { id: uuidv4(), username, text: msg })
    })
  })
}
