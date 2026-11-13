/**
 * React hooks for using your app's
 * [WebSocket](https://wasp.sh/docs/features/websockets) server from the
 * client. Only available if your app sets the
 * {@linkcode "@wasp/spec"!WebSocket | webSocket} field of the app spec.
 *
 * All the hooks are typed with the events and payloads you defined on the
 * server, in your `webSocketFn` (see
 * {@link "server/webSocket"!WebSocketDefinition | WebSocketDefinition}). Your
 * editor autocompletes the events and payloads, and you get type errors if you
 * make a mistake. If you don't type your `webSocketFn`, any event name is
 * allowed.
 *
 * See [Using the WebSocket on the client](https://wasp.sh/docs/features/websockets#using-the-websocket-on-the-client)
 * for a complete chat page example.
 *
 * @module
 */

import type { EventsMap } from "@socket.io/component-emitter";
import type { Socket } from "socket.io-client";

/**
 * Gives you access to the WebSocket connection.
 *
 * Wasp automatically connects to the WebSocket server when the app loads, so
 * you don't need to call `socket.connect()` or `socket.disconnect()`. If you
 * set `autoConnect: false` in the `webSocket` field of the app spec, call
 * them as needed.
 *
 * All components using `useSocket` share the same underlying `socket`. When
 * the user logs in or out, Wasp reconnects it if it's connected, so the server
 * sees the new session.
 *
 * @example
 * ```tsx
 * import { useSocket } from "wasp/client/webSocket";
 *
 * export const ChatPage = () => {
 *   const { socket, isConnected } = useSocket();
 *
 *   function sayHello() {
 *     socket.emit("chatMessage", "Hello!");
 *   }
 *   // ...
 * };
 * ```
 */
export declare function useSocket(): {
  /**
   * The [Socket.IO client `Socket`](https://socket.io/docs/v4/client-api/#socket),
   * for sending and receiving events.
   */
  socket: Socket<ServerToClientEvents, ClientToServerEvents>;
  /**
   * Whether the socket is connected. Useful for showing the connection status.
   */
  isConnected: boolean;
};

/**
 * Registers a handler for an event the server sends. It unregisters the
 * handler when the component unmounts.
 *
 * @param event The name of the event, as defined on the server.
 * @param handler The function to call with the event's payload.
 *
 * @example
 * ```tsx
 * import { useState } from "react";
 * import {
 *   useSocketListener,
 *   type ServerToClientPayload,
 * } from "wasp/client/webSocket";
 *
 * export const ChatPage = () => {
 *   const [messages, setMessages] = useState<
 *     ServerToClientPayload<"chatMessage">[]
 *   >([]);
 *
 *   useSocketListener("chatMessage", (message) => {
 *     setMessages((priorMessages) => [message, ...priorMessages]);
 *   });
 *   // ...
 * };
 * ```
 */
export declare function useSocketListener<
  Event extends keyof ServerToClientEvents,
>(event: Event, handler: ServerToClientEvents[Event]): void;

/**
 * The payload type of an event the server sends to the client.
 *
 * @typeParam Event The name of the event, as defined on the server.
 *
 * @example
 * ```ts
 * import { type ServerToClientPayload } from "wasp/client/webSocket";
 *
 * function logMessage(message: ServerToClientPayload<"chatMessage">) {
 *   console.log(message);
 * }
 * ```
 */
export type ServerToClientPayload<Event extends keyof ServerToClientEvents> =
  Parameters<ServerToClientEvents[Event]>[0];

/**
 * The payload type of an event the client sends to the server.
 *
 * @typeParam Event The name of the event, as defined on the server.
 *
 * @example
 * ```ts
 * import { type ClientToServerPayload } from "wasp/client/webSocket";
 *
 * const message: ClientToServerPayload<"chatMessage"> = "Hello!";
 * ```
 */
export type ClientToServerPayload<Event extends keyof ClientToServerEvents> =
  Parameters<ClientToServerEvents[Event]>[0];

/**
 * The events the server sends to the client, as a map from event names to
 * listener functions.
 *
 * In your app, it's the `ServerToClientEvents` type argument of the
 * {@link "server/webSocket"!WebSocketDefinition | WebSocketDefinition} you type
 * your `webSocketFn` with.
 */
interface ServerToClientEvents extends EventsMap {}

/**
 * The events the client sends to the server, as a map from event names to
 * listener functions.
 *
 * In your app, it's the `ClientToServerEvents` type argument of the
 * {@link "server/webSocket"!WebSocketDefinition | WebSocketDefinition} you type
 * your `webSocketFn` with.
 */
interface ClientToServerEvents extends EventsMap {}

export {};
