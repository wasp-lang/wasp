---
title: WebSockets
---
import useBaseUrl from '@docusaurus/useBaseUrl';

# WebSocket support
Wasp provides a fully integrated WebSocket experience by utilizing [Socket.IO](https://socket.io/) on the client and server. We handle making sure your URLs are correctly setup, CORS is enabled, and provide a useful `useSocket` abstraction for use in React components.

## Enable in Wasp file
Here, we specify we are using WebSockets by adding `webSocket` to our `app` and providing the required `fn`. You can optionally specify a list of `entities` for use in your context (like in operations).

```wasp title=todoApp.wasp
app todoApp {
  // ...

  webSocket: {
    fn: import { webSocketFn } from "@server/webSocket.js"
    // Optional: entities: [...]
  },,
}
```

## Optional: Add shared types for client and server use
If TypeScript is your thing, you can add shared interface definitions to use on both the client and server. This allows you to type all the messages you send and receive!

```ts title=src/shared/webSocket.ts
import { WaspSocketData } from '@wasp/universal/types'

export interface ServerToClientEvents {
  chatMessage: (msg: { id: string, username: string, text: string }) => void;
}

export interface ClientToServerEvents {
  chatMessage: (msg: string) => void;
}

export interface InterServerEvents { }

// Data that is attached to the socket.
// NOTE: Wasp automatically injects the JWT into the connection,
// and if present/valid, the server adds a user to the socket.
export interface SocketData extends WaspSocketData { }
```

## Add server support
On the server, you will get `io` argument and `context` for your WebSocket function. You can use this `io` object to register callbacks for all the regular Socket.IO events.

```ts title=src/server/webSocket.ts
import { v4 as uuidv4 } from 'uuid'
import { WebSocketDefinition } from '@wasp/webSocket'
import { ServerToClientEvents, ClientToServerEvents, InterServerEvents, SocketData } from '../shared/webSocket'

export const webSocketFn: WebSocketDefinition<
  ClientToServerEvents,
  ServerToClientEvents,
  InterServerEvents,
  SocketData
> = (io, context) => {
  io.on('connection', (socket) => {
    const username = socket.data.user?.email || socket.data.user?.username || 'unknown'
    console.log('a user connected: ', username)

    socket.on('chatMessage', async (msg) => {
      console.log('message: ', msg)
      io.emit('chatMessage', { id: uuidv4(), username, text: msg })
    })
  })
}
```

## Add client support
Client access to WebSockets is provided by the `useSocket` abstraction. It returns:
- `socket: Socket` for sending and receiving events.
- `isConnected: boolean` for showing a display of the Socket.IO connection status.
  - Note: Wasp automatically connects establishes a WebSocket connection from the client to the server, so you do not need to explicitly `socket.connect()` or `socket.disconnect()`. All components using `useSocket` share the same underlying `socket`.
- `refreshAuthToken: () => void` for refreshing the auth token on the `socket` when login status changes.
  - Note: This function is automatically called when components using `useSocket` mount. It will attach the JWT, if it exists, and parse it on the server. If a valid JWT existed, you will have a `socket.data.user` on the server.

```ts title=src/client/Chat.tsx
import React, { useEffect, useRef, useState } from 'react'
import { useSocket } from '@wasp/webSocket'
import { ClientToServerEvents, ServerToClientEvents } from '../shared/webSocket'

export const ProfilePage = () => {
  const [messages, setMessages] = useState<{ id: string, username: string, text: string }[]>([]);
  const { socket, isConnected } = useSocket<ServerToClientEvents, ClientToServerEvents>()
  const inputRef = useRef<HTMLInputElement>(null)

  useEffect(() => {
    socket.on('chatMessage', logMessage)

    return () => {
      socket.off('chatMessage', logMessage)
    }
  }, [])

  function logMessage(msg: { id: string, username: string, text: string }) {
    setMessages((priorMessages) => [msg, ...priorMessages])
  }

  function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()

    if (inputRef.current !== null) {
      socket.emit('chatMessage', inputRef.current.value)
      inputRef.current.value = ''
    }
  }

  const messageList = messages.map((msg) => <li key={msg.id}><em>{msg.username}</em>: {msg.text}</li>)
  const connectionIcon = isConnected ? '🟢' : '🔴'

  return (
    <>
      <h2>Chat</h2>
      <div>
        <form onSubmit={handleSubmit}>
          <div className="flex space-x-4 place-items-center">
            <div>{connectionIcon}</div>
            <div><input type="text" ref={inputRef} /></div>
            <div><button className="btn btn-primary" type="submit">Submit</button></div>
          </div>
        </form>
        <ul>{messageList}</ul>
      </div>
    </>
  )
}
```
