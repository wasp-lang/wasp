---
comments: true
last_checked_with_versions:
  Wasp: "0.21"
---

# WebSocket Namespaces

This guide shows you how to use Socket.IO namespaces with Wasp's WebSocket support for organizing your real-time communication channels.

## Understanding Namespaces

Wasp's built-in WebSocket support gives you a single default connection with type-safe events via `useSocket` and `useSocketListener` (see the [Web Sockets docs](../../advanced/web-sockets.md)). Namespaces are a Socket.IO feature that lets you split real-time logic over separate channels on a single shared connection. This is useful when you want to separate concerns, for example having a `/chat` namespace for chat-related events and a `/notifications` namespace for notification events.

When using namespaces, you bypass Wasp's built-in client hooks (`useSocket`, `useSocketListener`) and manage connections directly with `socket.io-client`. Wasp still handles the server-side setup and provides the `io` server instance.

## Setting up WebSocket with Namespaces

### 1. Configure WebSocket in main.wasp

Enable WebSocket in your Wasp configuration with `autoConnect: false` since you'll manage connections manually:

```wasp title="main.wasp"
app WebsocketTest {
  wasp: {
    version: "^0.21.0"
  },
  title: "websocket-test",
  // highlight-start
  webSocket: {
    fn: import { webSocketFn } from "@src/websocketSetup",
    autoConnect: false
  }
  // highlight-end
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import Main from "@src/MainPage"
}
```

### 2. Create the server-side WebSocket handler

Set up the namespace on the server side. The `webSocketFn` receives the Socket.IO `io` server and a `context` object with access to your entities:

```ts title="src/websocketSetup.ts" auto-js
import { type WebSocketDefinition } from "wasp/server/webSocket";

export const webSocketFn: WebSocketDefinition = (io, _context) => {
  // Create a namespace for messages
  const messagesNamespace = io.of("/messages");

  messagesNamespace.on("connection", (socket) => {
    console.log("Client connected to messages namespace");

    socket.on("chatMessage", (msg) => {
      console.log("message: ", msg);
      // Broadcast to all clients in the namespace
      messagesNamespace.emit("chatMessage", {
        id: crypto.randomUUID(),
        username: "User",
        text: msg,
      });
    });

    socket.on("disconnect", () => {
      console.log("Client disconnected from messages namespace");
    });
  });
};
```

### 3. Create client-side WebSocket utilities

Create a module to manage the namespace connection on the client. Since you're connecting to a custom namespace, you use `socket.io-client` directly instead of Wasp's built-in `useSocket` hook:

```ts title="src/websocketHooks.ts" auto-js
import { useEffect, useState } from "react";
import { io, type Socket } from "socket.io-client";
import { config } from "wasp/client";

const messagesSocket: Socket = io(`${config.apiUrl}/messages`, {
  transports: ["websocket"],
  // Vite pre-bundles socket.io-client which breaks autoConnect: https://github.com/vitejs/vite/issues/4798
  autoConnect: false,
});
messagesSocket.connect();

export function useMessagesSocket(): { socket: Socket; isConnected: boolean } {
  const [isConnected, setIsConnected] = useState(messagesSocket.connected);

  useEffect(() => {
    function onConnect() {
      setIsConnected(true);
    }

    function onDisconnect() {
      setIsConnected(false);
    }

    messagesSocket.on("connect", onConnect);
    messagesSocket.on("disconnect", onDisconnect);

    return () => {
      messagesSocket.off("connect", onConnect);
      messagesSocket.off("disconnect", onDisconnect);
    };
  }, []);

  return {
    socket: messagesSocket,
    isConnected,
  };
}

export function useSocketListener(
  socket: Socket,
  event: string,
  handler: (...args: any[]) => void,
) {
  useEffect(() => {
    socket.on(event, handler);
    return () => {
      socket.off(event, handler);
    };
  }, [event, handler, socket]);
}
```

### 4. Use the WebSocket in your component

Now use the hooks in your React component:

```tsx title="src/MainPage.tsx" auto-js
import { useMessagesSocket, useSocketListener } from "./websocketHooks";

const MainPage = () => {
  const { socket, isConnected } = useMessagesSocket();

  useSocketListener(socket, "chatMessage", (message) => {
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

## Multiple Namespaces

You can create multiple namespaces for different purposes:

```ts title="src/websocketSetup.ts" auto-js
export const webSocketFn: WebSocketDefinition = (io, _context) => {
  // Messages namespace
  const messagesNamespace = io.of("/messages");
  messagesNamespace.on("connection", (socket) => {
    // Handle messages events
  });

  // Notifications namespace
  const notificationsNamespace = io.of("/notifications");
  notificationsNamespace.on("connection", (socket) => {
    // Handle notification events
  });

  // Presence namespace
  const presenceNamespace = io.of("/presence");
  presenceNamespace.on("connection", (socket) => {
    // Handle presence events
  });
};
```

## Room Support within Namespaces

Namespaces can also use rooms for further organization:

```ts auto-js
messagesNamespace.on("connection", (socket) => {
  // Join a specific room
  socket.on("joinRoom", (roomId) => {
    socket.join(roomId);
  });

  // Send message to a specific room
  socket.on("roomMessage", ({ roomId, message }) => {
    messagesNamespace.to(roomId).emit("chatMessage", message);
  });
});
```

For more information about Socket.IO namespaces, see the [Socket.IO documentation](https://socket.io/docs/v4/namespaces/).
