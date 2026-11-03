/**
 * React hooks for using your app's
 * [WebSocket](https://wasp.sh/docs/features/websockets) server from the
 * client.
 *
 * All the hooks are typed with the events and payloads you defined on the
 * server, in your `webSocketFn` (see `WebSocketDefinition` in
 * `wasp/server/webSocket`). Your editor autocompletes the events and
 * payloads, and you get type errors if you make a mistake.
 *
 * @example
 * ```tsx
 * import React, { useState } from "react";
 * import {
 *   useSocket,
 *   useSocketListener,
 *   type ClientToServerPayload,
 *   type ServerToClientPayload,
 * } from "wasp/client/webSocket";
 *
 * export const ChatPage = () => {
 *   const [messageText, setMessageText] =
 *     useState<ClientToServerPayload<"chatMessage">>("");
 *   const [messages, setMessages] = useState<
 *     ServerToClientPayload<"chatMessage">[]
 *   >([]);
 *   // The "socket" instance is typed with the types you defined on the server.
 *   const { socket, isConnected } = useSocket();
 *
 *   // This is a type-safe event handler: "chatMessage" event and its payload
 *   // type are defined on the server.
 *   useSocketListener("chatMessage", logMessage);
 *
 *   function logMessage(msg: ServerToClientPayload<"chatMessage">) {
 *     setMessages((priorMessages) => [msg, ...priorMessages]);
 *   }
 *
 *   function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
 *     e.preventDefault();
 *     // This is a type-safe event emitter: "chatMessage" event and its
 *     // payload type are defined on the server.
 *     socket.emit("chatMessage", messageText);
 *     setMessageText("");
 *   }
 *
 *   // ...
 * };
 * ```
 *
 * @see [Using the WebSocket on the client](https://wasp.sh/docs/features/websockets#using-the-websocket-on-the-client)
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
 * All components using `useSocket` share the same underlying `socket`.
 *
 * @returns An object with:
 *
 * - `socket`: The Socket.IO client `Socket`, for sending and receiving
 *   events. It's typed with the events you defined on the server.
 * - `isConnected`: Whether the socket is connected. Useful for showing the
 *   connection status.
 *
 * @example
 * ```tsx
 * import { useSocket } from "wasp/client/webSocket";
 *
 * export const ChatPage = () => {
 *   const { socket, isConnected } = useSocket();
 *   // ...
 *   socket.emit("chatMessage", "Hello!");
 * };
 * ```
 *
 * @see [The `useSocket` hook](https://wasp.sh/docs/features/websockets#the-usesocket-hook)
 */
export declare function useSocket(): {
  socket: Socket<ServerToClientEvents, ClientToServerEvents>;
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
 *   useSocketListener("chatMessage", (msg) => {
 *     setMessages((priorMessages) => [msg, ...priorMessages]);
 *   });
 *   // ...
 * };
 * ```
 *
 * @see [The `useSocketListener` hook](https://wasp.sh/docs/features/websockets#the-usesocketlistener-hook)
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
 * function logMessage(msg: ServerToClientPayload<"chatMessage">) {
 *   console.log(msg);
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
 * `WebSocketDefinition` you type your `webSocketFn` with (from
 * `wasp/server/webSocket`). If you don't type it, any event name is allowed.
 */
interface ServerToClientEvents extends EventsMap {}

/**
 * The events the client sends to the server, as a map from event names to
 * listener functions.
 *
 * In your app, it's the `ClientToServerEvents` type argument of the
 * `WebSocketDefinition` you type your `webSocketFn` with (from
 * `wasp/server/webSocket`). If you don't type it, any event name is allowed.
 */
interface ClientToServerEvents extends EventsMap {}

export {};
