---
title: Web Sockets
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { CardLink } from '@site/src/components/CardLink';
import { ShowForTs } from '@site/src/components/TsJsHelpers';
import { Required } from '@site/src/components/Tag';

Wasp gives you a fully integrated WebSocket experience: a WebSocket server that lives inside your app's server, and `useSocket` and `useSocketListener` hooks for your React components.

Wasp takes care of the parts that are usually annoying: the connection uses your app's own URL, it reconnects when it drops, and it knows who is logged in.

To get started, you need to:

1. Define your WebSocket logic on the server.
2. Enable WebSockets in your Wasp file, and connect it with your server logic.
3. Use WebSockets on the client, in React, via `useSocket` and `useSocketListener`.
4. Optionally, type the WebSocket events and payloads for full-stack type safety.

Let's go through setting up WebSockets step by step, starting with enabling WebSockets in your Wasp file.

## Turn On WebSockets in Your Wasp File

We specify that we are using WebSockets by adding `webSocket` to our `app` and providing the required `fn`. You can optionally change the auto-connect behavior.

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { webSocketFn } from "./src/webSocket" with { type: "ref" }

export default app({
  name: "myApp",
  webSocket: {
    fn: webSocketFn,
    autoConnect: true, // optional, default: true
  },
  // ...
})
```

## Defining the Events Handler

Let's define the WebSockets server with all of the events and handler functions.

<ShowForTs>
  :::info Full-stack type safety
  Check this out: we'll define the event types and payloads on the server, and they will be **automatically exposed on the client**. This helps you avoid mistakes when emitting events or handling them.
  :::
</ShowForTs>

### `webSocketFn` Function {#websocketfn}

Your `webSocketFn` is a WebSocket definition you create with `defineWebSocket`. It's an object of hooks Wasp calls while a connection lives: when it opens, when the client sends you an event, and when it closes.

Every hook gets the `peer` (the client on the other end of the connection) and a `context` object that gives you access to all of the entities from your Wasp app. If a user is logged in, `peer.data.user` is who they are.

This is how we can define our `webSocketFn` function:

```ts title="src/webSocket.ts" auto-js
import { v4 as uuidv4 } from "uuid";
import {
  broadcast,
  defineWebSocket,
  type WaspSocketPeer,
} from "wasp/server/webSocket";

export const webSocketFn = defineWebSocket<
  // Typing your WebSocket definition with the events and payloads
  // gives you type safety on the client as well.
  ClientToServerEvents,
  ServerToClientEvents
>({
  open(peer) {
    console.log("a user connected: ", getUsername(peer));
  },

  events: {
    async chatMessage(peer, msg, context) {
      console.log("message: ", msg);
      broadcast("chatMessage", {
        id: uuidv4(),
        username: getUsername(peer),
        text: msg,
      });
      // You can also use your entities here:
      // await context.entities.SomeEntity.create({ someField: msg })
    },
  },

  close(peer) {
    console.log("a user disconnected: ", getUsername(peer));
  },
});

// Wasp resolves the logged-in user while the connection is being opened,
// so `peer.data.user` is already there when your hooks run.
function getUsername(peer: WaspSocketPeer<ServerToClientEvents>): string {
  return peer.data.user?.getFirstProviderUserId() ?? "Unknown";
}

interface ServerToClientEvents {
  chatMessage: (msg: { id: string; username: string; text: string }) => void;
}

interface ClientToServerEvents {
  chatMessage: (msg: string) => void;
}
```

:::info One payload per event
Every event carries exactly one payload. If you declare an event as `(a: string, b: number) => void`, only `a` is sent. Put everything an event needs into a single object instead.
:::

### Sending Events

There are three ways to send an event, depending on who should receive it:

| What you want                       | How to do it                                  |
| ----------------------------------- | --------------------------------------------- |
| Send to one client                  | `peer.send(event, payload)`                   |
| Send to everybody                   | `broadcast(event, payload)`                   |
| Send to everybody in a _topic_      | `publish(topic, event, payload)`              |
| Send to a topic, except this client | `peer.publishToOthers(topic, event, payload)` |

`broadcast` and `publish` are plain functions you import from `wasp/server/webSocket`, so you can also use them from a Query, an Action, or a Job:

```ts title="src/actions.ts" auto-js
import { broadcast } from "wasp/server/webSocket";
import { type PostAnnouncement } from "wasp/server/operations";

export const postAnnouncement: PostAnnouncement<
  { text: string },
  void
> = async (args, context) => {
  broadcast("announcement", { text: args.text });
};
```

### Topics (Rooms)

A topic is a named group of connections. Subscribe a client to a topic, and it receives everything published to it:

```ts title="src/webSocket.ts" auto-js
import { defineWebSocket, publish } from "wasp/server/webSocket";

export const webSocketFn = defineWebSocket<
  ClientToServerEvents,
  ServerToClientEvents
>({
  events: {
    joinRoom(peer, roomId) {
      peer.subscribe(roomId);
    },

    roomMessage(peer, { roomId, text }) {
      // Everybody in the room, including the sender.
      publish(roomId, "roomMessage", { text });
    },

    leaveRoom(peer, roomId) {
      peer.unsubscribe(roomId);
    },
  },
});
```

Use `peer.publishToOthers(...)` instead of `publish(...)` when the sender shouldn't receive its own message back.

### Refusing a Connection

By default, anybody can connect, and `peer.data.user` is `null` for clients that aren't logged in. If you want to turn some connections away, add an `upgrade` hook and throw a `Response` from it. The client never sees an open connection:

```ts title="src/webSocket.ts" auto-js
import { defineWebSocket } from "wasp/server/webSocket";

export const webSocketFn = defineWebSocket({
  upgrade(request, data) {
    if (data.user === null) {
      throw new Response("Unauthorized", { status: 401 });
    }
  },
  // ...
});
```

:::caution Your app has to be running as one instance
`broadcast` and `publish` only reach the clients connected to the process they run in. If you run your app on more than one instance, they won't reach the clients connected to the others. Wasp doesn't offer a way to connect the instances yet: follow [this issue](https://github.com/wasp-lang/wasp/issues/1228) if you need it.
:::

## Using the WebSocket On The Client

<ShowForTs>
  :::info Full-stack type safety
  All the hooks we use are typed with the events and payloads you defined on the server. VS Code will give you autocomplete for the events and payloads, and you will get type errors if you make a mistake.
  :::
</ShowForTs>

### The `useSocket` Hook

Client access to WebSockets is provided by the `useSocket` hook. It returns:

- `socket` for sending and receiving events.
- `isConnected: boolean` for showing a display of the connection status.
  - Note: Wasp automatically connects and establishes a WebSocket connection from the client to the server by default, so you do not need to explicitly `socket.connect()` or `socket.disconnect()`.
  - If you set `autoConnect: false` in your Wasp file, then you should call these as needed.

All components using `useSocket` share the same underlying `socket`.

Events you emit before the connection is open are sent as soon as it is, so you never have to wait for `isConnected` before emitting.

### The `useSocketListener` Hook

Additionally, there is a `useSocketListener: (event, callback) => void` hook which is used for registering event handlers. It takes care of unregistering the handler on unmount.

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/ChatPage.jsx"
    import React, { useState } from "react"
    import {
      useSocket,
      useSocketListener,
    } from "wasp/client/webSocket"

    export const ChatPage = () => {
      const [messageText, setMessageText] = useState("")
      const [messages, setMessages] = useState([])
      const { socket, isConnected } = useSocket()

      useSocketListener("chatMessage", logMessage)

      function logMessage(msg) {
        setMessages((priorMessages) => [msg, ...priorMessages])
      }

      function handleSubmit(e) {
        e.preventDefault()
        socket.emit("chatMessage", messageText)
        setMessageText("")
      }

      const messageList = messages.map((msg) => (
        <li key={msg.id}>
          <em>{msg.username}</em>: {msg.text}
        </li>
      ))
      const connectionIcon = isConnected ? "🟢" : "🔴"

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

    ```tsx title="src/ChatPage.tsx"
    import React, { useState } from "react"
    import {
      useSocket,
      useSocketListener,
      ServerToClientPayload,
    } from "wasp/client/webSocket"

    export const ChatPage = () => {
      const [messageText, setMessageText] = useState<
        // We are using a helper type to get the payload type for the "chatMessage" event.
        ClientToServerPayload<"chatMessage">
      >("")
      const [messages, setMessages] = useState<
        ServerToClientPayload<"chatMessage">[]
      >([])
      // The "socket" instance is typed with the types you defined on the server.
      const { socket, isConnected } = useSocket()

      // This is a type-safe event handler: "chatMessage" event and its payload type
      // are defined on the server.
      useSocketListener("chatMessage", logMessage)

      function logMessage(msg: ServerToClientPayload<"chatMessage">) {
        setMessages((priorMessages) => [msg, ...priorMessages])
      }

      function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
        e.preventDefault()
        // This is a type-safe event emitter: "chatMessage" event and its payload type
        // are defined on the server.
        socket.emit("chatMessage", messageText)
        setMessageText("")
      }

      const messageList = messages.map((msg) => (
        <li key={msg.id}>
          <em>{msg.username}</em>: {msg.text}
        </li>
      ))
      const connectionIcon = isConnected ? "🟢" : "🔴"

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

:::tip Let the client ask for the initial state
Your components register their listeners when they mount, which can be after the connection has already opened. An event your server sends from its `open` hook can therefore arrive before anybody is listening for it.

Instead of pushing the initial state from `open`, have the client ask for it (emit something like `askForStateUpdate` when the component mounts) and answer that event.
:::

## API Reference

<CardLink
  to="../api/@wasp.sh/spec/interfaces/WebSocket"
  kind="api"
  title="WebSocket"
  description="All the options for the webSocket field of the app spec."
/>
