---
last_update:
  date: 2023-10-05
title: WebSocket Namespaces
comments: true
---

# WebSocket Namespaces

This guide shows you how to use Socket.IO namespaces with Wasp's WebSocket support for organizing your real-time communication channels.

## Prerequisites

Make sure you have a Wasp project set up with WebSocket support. If you haven't, follow the [Getting Started](../../getting-started.md) guide first.

## Understanding Namespaces

Namespaces are a way to split the logic of your application over a single shared connection. This is useful when you want to separate concerns, for example having a `/chat` namespace for chat-related events and a `/notifications` namespace for notification events.

## Setting up WebSocket with Namespaces

### 1. Configure WebSocket in main.wasp

First, enable WebSocket in your Wasp configuration with `autoConnect: false` since we'll manage connections manually:

```wasp title="main.wasp"
app WebsocketTest {
  wasp: {
    version: "^0.15.0"
  },
  title: "websocket-test",
  webSocket: {
    fn: import { webSocketFn } from "@src/websocket.js",
    autoConnect: false
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import Main from "@src/MainPage.jsx"
}
```

### 2. Create the server-side WebSocket handler

Set up the namespace on the server side:

```ts title="src/websocket.ts"
import type { WebSocketDefinition } from "wasp/server/webSocket";
import { v4 as uuidv4 } from "uuid";

export const webSocketFn: WebSocketDefinition = (io) => {
  // Create a namespace for messages
  const messagesNamespace = io.of("/messages");

  messagesNamespace.on("connection", (socket) => {
    console.log("Client connected to messages namespace");

    socket.on("chatMessage", (msg) => {
      console.log("message: ", msg);
      // Broadcast to all clients in the namespace
      messagesNamespace.emit("chatMessage", {
        id: uuidv4(),
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

Don't forget to install uuid:

```bash
npm install uuid
npm install --save-dev @types/uuid
```

### 3. Create client-side WebSocket utilities

Create a module to manage the namespace connection on the client:

```ts title="src/websockets.ts"
import { useEffect, useState } from "react";
import { io, Socket } from "socket.io-client";
import { config } from "wasp/client";

// Create the socket connection to the messages namespace
let messagesSocket: Socket = io(`${config.apiUrl}/messages`);

export function useMessagesSocket() {
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
  }, [event, handler]);
}
```

### 4. Use the WebSocket in your component

Now use the hooks in your React component:

```jsx title="src/MainPage.jsx"
import "./Main.css";
import { useMessagesSocket, useSocketListener } from "./websockets";

const MainPage = () => {
  const { socket, isConnected } = useMessagesSocket();

  useSocketListener(socket, "chatMessage", (message) => {
    console.log("message received: ", message);
  });

  return (
    <div className="container">
      <main>
        <p>Status: {isConnected ? "Connected" : "Disconnected"}</p>
        <button onClick={() => socket.emit("chatMessage", "hello")}>
          Send message
        </button>
      </main>
    </div>
  );
};

export default MainPage;
```

## Multiple Namespaces

You can create multiple namespaces for different purposes:

```ts title="src/websocket.ts"
export const webSocketFn: WebSocketDefinition = (io) => {
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

```ts
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
