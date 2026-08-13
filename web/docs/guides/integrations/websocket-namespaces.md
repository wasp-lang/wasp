---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# WebSocket Channels

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.26" }} />

This guide shows you how to split your app's real-time communication into separate channels over Wasp's single WebSocket connection.

## Understanding Channels

Wasp's built-in WebSocket support gives you a single connection with type-safe events via `useSocket` and `useSocketListener` (see the [Web Sockets docs](../../advanced/web-sockets.md)). One connection is usually enough, but as your app grows you may want to keep unrelated real-time logic apart, for example chat messages and notifications.

You do that with **topics**: named groups of connections you subscribe to and publish to. A client only receives the events of the topics it is subscribed to, and everything still rides the one connection Wasp manages for you, so you keep `useSocket` and `useSocketListener`.

:::info Coming from Socket.IO?
Wasp used to build its WebSocket support on Socket.IO, where you would reach for namespaces here. There are no namespaces anymore. Topics cover the same "separate channels" use case, without a second connection to manage.
:::

## Setting up channels

### 1. Configure WebSocket in main.wasp.ts

Enable WebSocket in your Wasp spec:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import Main from "./src/MainPage" with { type: "ref" }
import { webSocketFn } from "./src/websocketSetup" with { type: "ref" }

export default app({
  name: "WebsocketTest",
  wasp: { version: "^0.26.0" },
  title: "websocket-test",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  // highlight-start
  webSocket: {
    fn: webSocketFn,
  },
  // highlight-end
  spec: [
    route("RootRoute", "/", page(Main)),
  ],
})
```

### 2. Create the server-side WebSocket handler

Let clients subscribe to the channels they care about, and publish each channel's events to it:

```ts title="src/websocketSetup.ts" auto-js
import { defineWebSocket, publish } from "wasp/server/webSocket";

export const webSocketFn = defineWebSocket<
  ClientToServerEvents,
  ServerToClientEvents
>({
  events: {
    subscribe(peer, channel) {
      peer.subscribe(channel);
    },

    unsubscribe(peer, channel) {
      peer.unsubscribe(channel);
    },

    chatMessage(peer, text) {
      publish("messages", "chatMessage", {
        id: crypto.randomUUID(),
        username: peer.data.user?.getFirstProviderUserId() ?? "Unknown",
        text,
      });
    },
  },
});

interface ClientToServerEvents {
  subscribe: (channel: string) => void;
  unsubscribe: (channel: string) => void;
  chatMessage: (text: string) => void;
}

interface ServerToClientEvents {
  chatMessage: (msg: {
    id: string;
    username: string;
    text: string;
  }) => void;
  notification: (msg: { text: string }) => void;
}
```

Note that `publish` reaches everybody subscribed to the topic, the sender included. If you want to exclude the sender, use `peer.publishToOthers(topic, event, payload)` instead.

### 3. Use the channel in your component

Subscribe when the component mounts, and listen for the channel's events with Wasp's hooks:

```tsx title="src/MainPage.tsx" auto-js
import { useEffect } from "react";
import { useSocket, useSocketListener } from "wasp/client/webSocket";

const MainPage = () => {
  const { socket, isConnected } = useSocket();

  useEffect(() => {
    socket.emit("subscribe", "messages");
    return () => socket.emit("unsubscribe", "messages");
  }, [socket]);

  useSocketListener("chatMessage", (message) => {
    console.log("message received: ", message);
  });

  return (
    <main>
      <p>Status: {isConnected ? "Connected" : "Disconnected"}</p>
      <button onClick={() => socket.emit("chatMessage", "hello")}>
        Send message
      </button>
    </main>
  );
};

export default MainPage;
```

Events you emit before the connection is open are sent as soon as it opens, so you don't have to wait for `isConnected` before subscribing.

## Multiple Channels

Nothing stops a connection from being subscribed to several topics at once:

```ts title="src/websocketSetup.ts" auto-js
import { defineWebSocket } from "wasp/server/webSocket";

export const webSocketFn = defineWebSocket({
  open(peer) {
    peer.subscribe("messages");
    peer.subscribe("notifications");
    peer.subscribe("presence");
  },
});
```

Since every event flows over the same connection, give each channel its own event names (`chatMessage`, `notification`, `presenceUpdate`, ...) instead of reusing one name across channels.

## Rooms within a channel

Topics are just strings, so you can name them per room:

```ts title="src/websocketSetup.ts" auto-js
import { defineWebSocket } from "wasp/server/webSocket";

export const webSocketFn = defineWebSocket<
  ClientToServerEvents,
  ServerToClientEvents
>({
  events: {
    joinRoom(peer, roomId) {
      peer.subscribe(`messages:${roomId}`);
    },

    roomMessage(peer, { roomId, text }) {
      peer.publishToOthers(`messages:${roomId}`, "chatMessage", { text });
    },
  },
});
```

## Sending from the rest of your server

`publish` and `broadcast` are plain functions, so a Query, an Action or a Job can send events to a channel too:

```ts title="src/actions.ts" auto-js
import { type NotifyEveryone } from "wasp/server/operations";
import { publish } from "wasp/server/webSocket";

export const notifyEveryone: NotifyEveryone<{ text: string }, void> = async (
  args,
  context,
) => {
  publish("notifications", "notification", { text: args.text });
};
```

For more about Wasp's WebSocket API, see the [Web Sockets docs](../../advanced/web-sockets.md).
