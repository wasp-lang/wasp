---
title: Web Sockets
---
import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs } from '@site/src/components/TsJsHelpers';
import { Required } from '@site/src/components/Tag';

Wasp provides a fully integrated WebSocket experience by utilizing [Socket.IO](https://socket.io/) on the client and server. 

We handle making sure your URLs are correctly setup, CORS is enabled, and provide a useful `useSocket` and `useSocketListener` abstractions for use in React components.

To get started, you need to:
1. Define your WebSocket logic on the server.
2. Enable WebSockets in your Wasp file, and connect it with your server logic.
3. Use WebSockets on the client, in React, via `useSocket` and `useSocketListener`.
4. Optionally, type the WebSocket events and payloads for full-stack type safety.

Let's go through setting up WebSockets step by step, starting with enabling WebSockets in your Wasp file.

## Turn On WebSockets in Your Wasp File
We specify that we are using WebSockets by adding `webSocket` to our `app` and providing the required `fn`. You can optionally change the auto-connect behavior.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=todoApp.wasp
app todoApp {
  // ...

  webSocket: {
    fn: import { webSocketFn } from "@src/webSocket",
    autoConnect: true, // optional, default: true
  },
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=todoApp.wasp
app todoApp {
  // ...

  webSocket: {
    fn: import { webSocketFn } from "@src/webSocket",
    autoConnect: true, // optional, default: true
  },
}
```
</TabItem>
</Tabs>

## Defining the Events Handler
Let's define the WebSockets server with all of the events and handler functions.

<ShowForTs>

:::info Full-stack type safety
Check this out: we'll define the event types and payloads on the server, and they will be **automatically exposed on the client**. This helps you avoid mistakes when emitting events or handling them.
:::
</ShowForTs>

### `webSocketFn` Function
On the server, you will get Socket.IO `io: Server` argument and `context` for your WebSocket function. The `context` object give you access to all of the entities from your Wasp app. 

You can use this `io` object to register callbacks for all the regular [Socket.IO events](https://socket.io/docs/v4/server-api/).  Also, if a user is logged in, you will have a `socket.data.user` on the server.

This is how we can define our `webSocketFn` function:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```ts title=src/webSocket.js
import { v4 as uuidv4 } from 'uuid'
import { getFirstProviderUserId } from 'wasp/auth'

export const webSocketFn = (io, context) => {
  io.on('connection', (socket) => {
    const username = getFirstProviderUserId(socket.data.user) ?? 'Unknown'
    console.log('a user connected: ', username)

    socket.on('chatMessage', async (msg) => {
      console.log('message: ', msg)
      io.emit('chatMessage', { id: uuidv4(), username, text: msg })
      // You can also use your entities here:
      // await context.entities.SomeEntity.create({ someField: msg })
    })
  })
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title=src/webSocket.ts
import { v4 as uuidv4 } from 'uuid'
import { getFirstProviderUserId } from 'wasp/auth'
import { type WebSocketDefinition, type WaspSocketData } from 'wasp/server/webSocket'

export const webSocketFn: WebSocketFn = (io, context) => {
  io.on('connection', (socket) => {
    const username = getFirstProviderUserId(socket.data.user) ?? 'Unknown'
    console.log('a user connected: ', username)

    socket.on('chatMessage', async (msg) => {
      console.log('message: ', msg)
      io.emit('chatMessage', { id: uuidv4(), username, text: msg })
      // You can also use your entities here:
      // await context.entities.SomeEntity.create({ someField: msg })
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
</TabItem>
</Tabs>

## Using the WebSocket On The Client

<ShowForTs>

:::info Full-stack type safety
All the hooks we use are typed with the events and payloads you defined on the server. VS Code will give you autocomplete for the events and payloads, and you will get type errors if you make a mistake.
:::
</ShowForTs>

### The `useSocket` Hook

Client access to WebSockets is provided by the `useSocket` hook. It returns:
- `socket: Socket` for sending and receiving events.
- `isConnected: boolean` for showing a display of the Socket.IO connection status.
  - Note: Wasp automatically connects and establishes a WebSocket connection from the client to the server by default, so you do not need to explicitly `socket.connect()` or `socket.disconnect()`. 
  - If you set `autoConnect: false` in your Wasp file, then you should call these as needed.

All components using `useSocket` share the same underlying `socket`.

### The `useSocketListener` Hook

Additionally, there is a `useSocketListener: (event, callback) => void` hook which is used for registering event handlers. It takes care of unregistering the handler on unmount.


<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title=src/ChatPage.jsx
import React, { useState } from 'react'
import {
  useSocket,
  useSocketListener,
} from 'wasp/client/webSocket'

export const ChatPage = () => {
  const [messageText, setMessageText] = useState('')
  const [messages, setMessages] = useState([])
  const { socket, isConnected } = useSocket()

  useSocketListener('chatMessage', logMessage)

  function logMessage(msg) {
    setMessages((priorMessages) => [msg, ...priorMessages])
  }

  function handleSubmit(e) {
    e.preventDefault()
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
</TabItem>
<TabItem value="ts" label="TypeScript">

Wasp's **full-stack type safety** kicks in here: all the event types and payloads are automatically inferred from the server and are available on the client.

You can additionally use the `ClientToServerPayload` and `ServerToClientPayload` helper types to get the payload type for a specific event.

```tsx title=src/ChatPage.tsx
import React, { useState } from 'react'
import {
  useSocket,
  useSocketListener,
  ServerToClientPayload,
} from 'wasp/client/webSocket'

export const ChatPage = () => {
  const [messageText, setMessageText] = useState<
    // We are using a helper type to get the payload type for the "chatMessage" event.
    ClientToServerPayload<'chatMessage'>
  >('')
  const [messages, setMessages] = useState<
    ServerToClientPayload<'chatMessage'>[]
  >([])
  // The "socket" instance is typed with the types you defined on the server.
  const { socket, isConnected } = useSocket()

  // This is a type-safe event handler: "chatMessage" event and its payload type
  // are defined on the server.
  useSocketListener('chatMessage', logMessage)

  function logMessage(msg: ServerToClientPayload<'chatMessage'>) {
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
</TabItem>
</Tabs>

## API Reference

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=todoApp.wasp
app todoApp {
  // ...

  webSocket: {
    fn: import { webSocketFn } from "@src/webSocket",
    autoConnect: true, // optional, default: true
  },
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=todoApp.wasp
app todoApp {
  // ...

  webSocket: {
    fn: import { webSocketFn } from "@src/webSocket",
    autoConnect: true, // optional, default: true
  },
}
```
</TabItem>
</Tabs>

The `webSocket` dict has the following fields:

- `fn: WebSocketFn` <Required />

  The function that defines the WebSocket events and handlers.

- `autoConnect: bool`

  Whether to automatically connect to the WebSocket server. Default: `true`.
