---
title: WebSockets
---
import useBaseUrl from '@docusaurus/useBaseUrl';

# WebSocket support
Wasp provides a fully integrated WebSocket experience by utilizing [Socket.IO](https://socket.io/) on the client and server. 

We handle making sure your URLs are correctly setup, CORS is enabled, and provide a useful `useSocket` and `useSocketListener` abstractions for use in React components.

To get started, you need to:
1. Define your WebSocket logic on the server.
2. Declare you are using WebSockets in your Wasp file, and connect it with your server logic.
3. Use WebSockets on the client, in React, via `useSocket` and `useSocketListener`.
4. Optionally, type the WebSocket events and payloads for full-stack type safety.

We will cover all the steps above, but in an order that makes it easier to explain new concepts.

## Turn on WebSockets in your Wasp file
We specify that we are using WebSockets by adding `webSocket` to our `app` and providing the required `fn`. You can optionally specify a list of `entities` for use in your context (like in Operations), as well as changing the auto-connect behavior (on by default).

```wasp title=todoApp.wasp
app todoApp {
  // ...

  webSocket: {
    fn: import { webSocketFn } from "@server/webSocket.js",
    entities: [], // optional
    autoConnect: true, // optional, default: true
  },
}
```

## Define the type-safe WebSocket server
Let's define the server with all of the events and handler functions.

If you are using TypeScript, you can define event names with the matching payload types on the server and have those types exposed automatically on the client. This allows you avoid making mistakes when emitting events or handling them.

### Defining the events handler
On the server, you will get Socket.IO `io: Server` argument and `context` for your WebSocket function, which contains your entities (like Operations). You can use this `io` object to register callbacks for all the regular [Socket.IO events](https://socket.io/docs/v4/server-api/). 

Lastly, if a user is logged in, you will have a `socket.data.user` on the server.

```ts title=src/server/webSocket.ts
import type { WebSocketDefinition, WaspSocketData } from '@wasp/webSocket'
import { v4 as uuidv4 } from 'uuid'

export const webSocketFn: WebSocketFn = (io, context) => {
  io.on('connection', (socket) => {
    const username = socket.data.user?.email || socket.data.user?.username || 'unknown'
    console.log('a user connected: ', username)

    socket.on('chatMessage', async (msg) => {
      console.log('message: ', msg)
      io.emit('chatMessage', { id: uuidv4(), username, text: msg })
    })
  })
}

// Typing our WebSocket function with the events and payloads
// allows us to get type safety on the client as well

type WebSocketFn = WebSocketDefinition<
  ClientToServerEvents,
  ServerToClientEvents,
  InterServerEvents,
  SocketData
>

interface ServerToClientEvents {
  chatMessage: (msg: { id: string, username: string, text: string }) => void;
}

interface ClientToServerEvents {
  chatMessage: (msg: string) => void;
}

interface InterServerEvents {}

// Data that is attached to the socket.
// NOTE: Wasp automatically injects the JWT into the connection,
// and if present/valid, the server adds a user to the socket.
interface SocketData extends WaspSocketData {}
```

## Using the WebSocket on the client
Client access to WebSockets is provided by the `useSocket` hook. It returns:
- `socket: Socket` for sending and receiving events.
- `isConnected: boolean` for showing a display of the Socket.IO connection status.
  - Note: Wasp automatically connects and establishes a WebSocket connection from the client to the server by default, so you do not need to explicitly `socket.connect()` or `socket.disconnect()`. 
  - If you set `autoConnect: false` in your Wasp file, then you should call these as needed.

Additionally, there is a `useSocketListener: (event, callback) => void` hook which is used for registering event handlers. It takes care of unregistering on unmount.

All components using `useSocket` share the same underlying `socket`.

```tsx title=src/client/ChatPage.tsx
import React, { useState } from 'react'
import { useSocket, useSocketListener } from '@wasp/webSocket'

export const ChatPage = () => {
  const [messageText, setMessageText] = useState('')
  const [messages, setMessages] = useState<
    { id: string; username: string; text: string }[]
  >([])
  // The "socket" instance is typed with the types you defined on the server.
  const { socket, isConnected } = useSocket()

  // This is a type-safe event handler: "chatMessage" event and its payload type
  // are defined on the server.
  useSocketListener('chatMessage', logMessage)

  function logMessage(msg: { id: string; username: string; text: string }) {
    setMessages((priorMessages) => [msg, ...priorMessages])
  }

  function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()
    // This is a type-safe event emitter: "chatMessage" event and its payload type
    // are defined on the server.
    socket.emit('chatMessage', messageText)
    setMessageText('')
  }

  const messageList = messages.map((msg) => (
    <li key={msg.id}>
      <em>{msg.username}</em>: {msg.text}
    </li>
  ))
  const connectionIcon = isConnected ? '🟢' : '🔴'

  return (
    <>
      <h2>Chat {connectionIcon}</h2>
      <div>
        <form onSubmit={handleSubmit}>
          <div>
            <div>
              <input
                type="text"
                value={messageText}
                onChange={(e) => setMessageText(e.target.value)}
              />
            </div>
            <div>
              <button type="submit">Submit</button>
            </div>
          </div>
        </form>
        <ul>{messageList}</ul>
      </div>
    </>
  )
}
```
