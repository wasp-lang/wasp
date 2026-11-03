/**
 * Types for implementing your app's
 * [WebSocket](https://wasp.sh/docs/features/websockets) server with
 * [Socket.IO](https://socket.io/docs/v4/server-api/).
 *
 * The event types you define on the server are exposed on the client, in
 * `wasp/client/webSocket`.
 *
 * @see [WebSockets](https://wasp.sh/docs/features/websockets)
 *
 * @module
 */

import type { DefaultEventsMap, EventsMap } from "@socket.io/component-emitter";
import type { Server } from "socket.io";
import type { AuthUser } from "../auth";

/**
 * The type of the function you pass to the `webSocket.fn` field of the app
 * spec. Wasp calls it on server start to define your WebSocket server's
 * events and handlers.
 *
 * It receives two arguments:
 *
 * 1. `io`: The Socket.IO `Server`. Use it to register callbacks for all the
 *    regular [Socket.IO events](https://socket.io/docs/v4/server-api/). If a
 *    user is logged in, `socket.data.user` has their {@link AuthUser} data.
 * 2. `context`: An object whose `entities` field has the Prisma API for all
 *    the Entities in your app, keyed by the Entity's name (e.g.
 *    `context.entities.Task` exposes `prisma.task`).
 *
 * Typing your function with the event types lets you get full-stack type
 * safety: the client's `useSocket` and `useSocketListener` hooks, from
 * `wasp/client/webSocket`, use the same event types.
 *
 * @typeParam ClientToServerEvents The events the client sends to the
 *   server, as a map from event names to listener functions.
 * @typeParam ServerToClientEvents The events the server sends to the
 *   client, as a map from event names to listener functions.
 * @typeParam InterServerEvents The events sent between servers.
 * @typeParam SocketData The type of `socket.data`. It must extend
 *   {@link WaspSocketData}.
 *
 * @example
 * ```ts
 * import { v4 as uuidv4 } from "uuid";
 * import {
 *   type WebSocketDefinition,
 *   type WaspSocketData,
 * } from "wasp/server/webSocket";
 *
 * export const webSocketFn: WebSocketFn = (io, context) => {
 *   io.on("connection", (socket) => {
 *     const username = socket.data.user?.getFirstProviderUserId() ?? "Unknown";
 *     console.log("a user connected: ", username);
 *
 *     socket.on("chatMessage", async (msg) => {
 *       console.log("message: ", msg);
 *       io.emit("chatMessage", { id: uuidv4(), username, text: msg });
 *       // You can also use your entities here:
 *       // await context.entities.SomeEntity.create({ someField: msg })
 *     });
 *   });
 * };
 *
 * // Typing our WebSocket function with the events and payloads
 * // allows us to get type safety on the client as well
 * type WebSocketFn = WebSocketDefinition<
 *   ClientToServerEvents,
 *   ServerToClientEvents,
 *   InterServerEvents,
 *   SocketData
 * >;
 *
 * interface ServerToClientEvents {
 *   chatMessage: (msg: { id: string; username: string; text: string }) => void;
 * }
 *
 * interface ClientToServerEvents {
 *   chatMessage: (msg: string) => void;
 * }
 *
 * interface InterServerEvents {}
 *
 * // Data that is attached to the socket.
 * // NOTE: Wasp automatically injects the JWT into the connection,
 * // and if present/valid, the server adds a user to the socket.
 * interface SocketData extends WaspSocketData {}
 * ```
 *
 * @see [The `webSocketFn` function](https://wasp.sh/docs/features/websockets#websocketfn)
 */
export type WebSocketDefinition<
  ClientToServerEvents extends EventsMap = DefaultEventsMap,
  ServerToClientEvents extends EventsMap = DefaultEventsMap,
  InterServerEvents extends EventsMap = DefaultEventsMap,
  SocketData extends WaspSocketData = WaspSocketData,
> = (
  io: Server<
    ClientToServerEvents,
    ServerToClientEvents,
    InterServerEvents,
    SocketData
  >,
  context: WebSocketContext,
) => Promise<void> | void;

/**
 * The `context` Wasp passes to your {@link WebSocketDefinition} function.
 */
type WebSocketContext = {
  /**
   * The Prisma API for each Entity in your app, keyed by the Entity's name.
   * For example, `context.entities.Task` exposes `prisma.task` from
   * [Prisma's CRUD API](https://www.prisma.io/docs/orm/prisma-client/queries/crud).
   */
  entities: { [EntityName: string]: any };
};

/**
 * The data Wasp attaches to each socket, in `socket.data`. Extend it to type
 * your own socket data.
 *
 * Wasp automatically sends the user's session with the connection, and if
 * it's valid, the server adds the user to the socket.
 */
export interface WaspSocketData {
  /**
   * The logged-in user, or `undefined` if the connection isn't
   * authenticated. Only present if your app uses auth.
   */
  user?: AuthUser;
}

export {};
