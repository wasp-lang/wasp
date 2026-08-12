{{={= =}=}}
import type {
  WebSocketHooks,
  WebSocketMessage,
  WebSocketPeer,
} from 'nitro/h3'

import { prisma } from '../index'
import type { FromRegister } from '../../types/register'
{=# isAuthEnabled =}
import { makeAuthUserIfPossible, type AuthUser } from '../../auth/user.js'
import { getSessionAndUserFromSessionId } from '../auth/session.js'
{=/ isAuthEnabled =}

/**
 * Everything Wasp sends over a websocket is one JSON object: an event name and
 * a payload. Both sides of the connection know the shape of every event from
 * the two event maps your websocket definition is typed with.
 */
type Envelope = {
  event: string
  payload?: unknown
}

/**
 * The Wasp-only subprotocol both sides always offer/accept. It is what the
 * server echoes back on the upgrade: a client that offered a subprotocol closes
 * the connection when the server picks none, and echoing this one back (instead
 * of the auth one) keeps the session ID out of the response headers.
 */
const waspSubProtocol = 'wasp'

/**
 * The prefix of the subprotocol carrying the session ID. Browsers can't set
 * headers on a `WebSocket`, and the subprotocol list is the only part of the
 * handshake that reaches us as a header (instead of the URL, which proxies and
 * access logs happily record).
 *
 * It must not start with `vite-`: Vite's dev server keeps those for itself and
 * silently swallows the upgrade.
 */
const authSubProtocolPrefix = 'wasp-auth.'

/** The query parameter that carries the session ID for clients that can't set a subprotocol. */
const sessionIdQueryParamName = 'sessionId'

/**
 * What an event map looks like: an event name for every key, and the function
 * shape of what it carries for every value.
 *
 * The values are `any` on purpose. TypeScript only lets a type it can't index
 * by an arbitrary string through an index signature of `any`, and event maps
 * are meant to be written as interfaces.
 */
// PRIVATE API (SDK)
export interface EventsMap {
  [event: string]: any
}

// PRIVATE API (SDK)
export interface DefaultEventsMap {
  [event: string]: (...args: any[]) => void
}

/**
 * The single value an event carries. Wasp's envelope has room for exactly one,
 * so an event declared as `(a: A, b: B) => void` only ever carries `a`.
 */
// PRIVATE API (SDK)
export type EventPayload<
  Events extends EventsMap,
  Event extends keyof Events
> = Events[Event] extends (...args: infer Args) => any
  ? Args extends []
    ? undefined
    : Args[0]
  : never

/** The argument list of the functions sending an event, so that events without a payload take none. */
// PRIVATE API (SDK)
export type EventArgs<
  Events extends EventsMap,
  Event extends keyof Events
> = undefined extends EventPayload<Events, Event>
  ? [payload?: EventPayload<Events, Event>]
  : [payload: EventPayload<Events, Event>]

// PUBLIC API
export type WaspSocketData = {
  {=# isAuthEnabled =}
  /** The user the connection authenticated as, or `null` for an anonymous one. */
  user: AuthUser | null
  {=/ isAuthEnabled =}
}

// PUBLIC API
export type WebSocketContext = {
  entities: {
    {=# allEntities =}
    {= name =}: typeof prisma.{= prismaIdentifier =},
    {=/ allEntities =}
  }
}

const context: WebSocketContext = {
  entities: {
    {=# allEntities =}
    {= name =}: prisma.{= prismaIdentifier =},
    {=/ allEntities =}
  }
}

/**
 * One connected client.
 *
 * It wraps the connection Nitro hands us (reachable as {@linkcode raw}) with
 * Wasp's envelope and your event types.
 */
// PUBLIC API
export class WaspSocketPeer<
  ServerToClientEvents extends EventsMap = DefaultEventsMap
> {
  constructor(
    /**
     * The underlying [crossws peer](https://crossws.h3.dev/guide/peer), for
     * everything this class doesn't wrap (the upgrade `request`, the
     * `remoteAddress`, sending raw frames, ...).
     */
    public readonly raw: WebSocketPeer
  ) {}

  /** Identifies the connection. Stable for as long as it is open. */
  get id(): string {
    return this.raw.id
  }

  /** What Wasp (and your `upgrade` hook) attached to the connection while it was being opened. */
  get data(): WaspSocketData {
    return this.raw.context as WaspSocketData
  }

  /** The topics this connection is subscribed to. */
  get topics(): Set<string> {
    return this.raw.topics
  }

  /** Sends an event to this connection only. */
  send<Event extends keyof ServerToClientEvents>(
    event: Event,
    ...args: EventArgs<ServerToClientEvents, Event>
  ): void {
    this.raw.send(encodeEnvelope(event, (args as unknown[])[0]))
  }

  /**
   * Sends an event to every connection subscribed to `topic`, **except this
   * one**. Follow it with {@linkcode send} to include this one too, or use the
   * module's `publish` (which never excludes anybody).
   */
  publishToOthers<Event extends keyof ServerToClientEvents>(
    topic: string,
    event: Event,
    ...args: EventArgs<ServerToClientEvents, Event>
  ): void {
    this.raw.publish(topic, encodeEnvelope(event, (args as unknown[])[0]))
  }

  /** Subscribes this connection to a topic (the equivalent of joining a room). */
  subscribe(topic: string): void {
    this.raw.subscribe(topic)
  }

  /** Unsubscribes this connection from a topic. */
  unsubscribe(topic: string): void {
    this.raw.unsubscribe(topic)
  }

  /** Closes the connection. */
  close(code?: number, reason?: string): void {
    this.raw.close(code, reason)
  }
}

/**
 * Your app's websocket, as a set of hooks Wasp calls while a connection lives.
 *
 * Type it with two event maps: what the client sends you, and what you send the
 * client. Both are written as functions of their payload, e.g.
 * `{ chatMessage: (message: string) => void }`.
 */
// PUBLIC API
export type WebSocketDefinition<
  ClientToServerEvents extends EventsMap = DefaultEventsMap,
  ServerToClientEvents extends EventsMap = DefaultEventsMap
> = {
  /**
   * Runs before the connection is opened, on the HTTP request asking for it.
   *
   * Wasp has already resolved the connection's `user` into `data` by then, and
   * whatever else you put there is on `peer.data` for the rest of the
   * connection. Throw a `Response` to refuse the connection: the client never
   * sees an open socket.
   */
  upgrade?: (
    request: Request,
    data: WaspSocketData,
    context: WebSocketContext
  ) => void | Promise<void>

  /** Runs once the connection is open. */
  open?: (
    peer: WaspSocketPeer<ServerToClientEvents>,
    context: WebSocketContext
  ) => void | Promise<void>

  /** Runs when the client sends the matching event. */
  events?: {
    [Event in keyof ClientToServerEvents]?: (
      peer: WaspSocketPeer<ServerToClientEvents>,
      payload: EventPayload<ClientToServerEvents, Event>,
      context: WebSocketContext
    ) => void | Promise<void>
  }

  /** Runs when the connection is closed, by either side. */
  close?: (
    peer: WaspSocketPeer<ServerToClientEvents>,
    details: { code?: number; reason?: string },
    context: WebSocketContext
  ) => void | Promise<void>

  /** Runs when the connection fails. It is closed right after. */
  error?: (
    peer: WaspSocketPeer<ServerToClientEvents>,
    error: Error,
    context: WebSocketContext
  ) => void | Promise<void>

  /**
   * Never set: it is only here so that Wasp can read your event types off your
   * definition and type your client code with them.
   */
  readonly _eventTypes?: [ClientToServerEvents, ServerToClientEvents]
}

/**
 * Declares your app's websocket. Give it your event maps and it types every
 * hook for you.
 */
// PUBLIC API
export function defineWebSocket<
  ClientToServerEvents extends EventsMap = DefaultEventsMap,
  ServerToClientEvents extends EventsMap = DefaultEventsMap
>(
  definition: WebSocketDefinition<ClientToServerEvents, ServerToClientEvents>
): WebSocketDefinition<ClientToServerEvents, ServerToClientEvents> {
  return definition
}

/**
 * Every connection currently open, so that the rest of your server (an action,
 * a job, ...) can send events to them.
 *
 * It lives on `globalThis` because in development your server's modules are
 * re-executed on every change, and a module-level registry would fork.
 *
 * NOTE: it only knows about the connections of the process it runs in. An app
 * running on more than one instance needs a message broker between them, which
 * Wasp doesn't offer yet.
 */
const connectedPeersKey = Symbol.for('wasp.webSocket.connectedPeers')
const globalProperties = globalThis as unknown as Record<
  symbol,
  Set<WebSocketPeer> | undefined
>
const connectedPeers: Set<WebSocketPeer> = (globalProperties[
  connectedPeersKey
] ??= new Set())

/**
 * Sends an event to every connected client, from anywhere in your server.
 *
 * The equivalent of `io.emit(...)` in the socket.io API Wasp used before.
 */
// PUBLIC API
export function broadcast<Event extends keyof ServerToClientEvents>(
  event: Event,
  ...args: EventArgs<ServerToClientEvents, Event>
): void {
  sendToPeers(connectedPeers, event, (args as unknown[])[0])
}

/**
 * Sends an event to every client subscribed to `topic`, from anywhere in your
 * server. Unlike `peer.publishToOthers(...)`, it excludes nobody.
 *
 * The equivalent of `io.to(room).emit(...)` in the socket.io API Wasp used
 * before.
 */
// PUBLIC API
export function publish<Event extends keyof ServerToClientEvents>(
  topic: string,
  event: Event,
  ...args: EventArgs<ServerToClientEvents, Event>
): void {
  const subscribers = [...connectedPeers].filter((peer) =>
    peer.topics.has(topic)
  )
  sendToPeers(subscribers, event, (args as unknown[])[0])
}

/**
 * Turns your app's websocket definition into the hooks Nitro's websocket route
 * expects: it authenticates the connection, keeps track of it, and turns the
 * messages it carries into your typed events.
 */
// PRIVATE API (framework)
export function createWebSocketHooks<
  ClientToServerEvents extends EventsMap,
  ServerToClientEvents extends EventsMap
>(
  definition: WebSocketDefinition<ClientToServerEvents, ServerToClientEvents>
): Partial<WebSocketHooks> {
  const peerFor = makePeerCache<ServerToClientEvents>()

  return {
    async upgrade(request) {
      const data: WaspSocketData = {
        {=# isAuthEnabled =}
        user: await getUserFromUpgradeRequest(request),
        {=/ isAuthEnabled =}
      }

      // Anything this throws (a `Response`, most usefully) refuses the upgrade.
      await definition.upgrade?.(request, data, context)

      const offeredSubProtocols = getOfferedSubProtocols(request)
      return {
        // Nitro negotiates no subprotocol on its own, and a client that offered
        // one closes the connection when the answer picks none. Undocumented,
        // and the only lever we have over it.
        protocol: offeredSubProtocols.includes(waspSubProtocol)
          ? waspSubProtocol
          : offeredSubProtocols[0],
        context: data,
      }
    },

    async open(rawPeer) {
      connectedPeers.add(rawPeer)
      await runHook('open', () => definition.open?.(peerFor(rawPeer), context))
    },

    async message(rawPeer, message) {
      const envelope = decodeEnvelope(message)
      if (envelope === null) {
        return
      }

      const handler = definition.events?.[
        envelope.event as keyof ClientToServerEvents
      ] as EventHandler<ServerToClientEvents> | undefined
      if (handler === undefined) {
        return
      }

      await runHook(`event '${envelope.event}'`, () =>
        handler(peerFor(rawPeer), envelope.payload, context)
      )
    },

    async close(rawPeer, details) {
      connectedPeers.delete(rawPeer)
      await runHook('close', () =>
        definition.close?.(peerFor(rawPeer), details, context)
      )
    },

    async error(rawPeer, error) {
      await runHook('error', () =>
        definition.error?.(peerFor(rawPeer), error as unknown as Error, context)
      )
    },
  }
}

/** One of your event handlers, with the payload left untyped (only you know its event). */
type EventHandler<ServerToClientEvents extends EventsMap> = (
  peer: WaspSocketPeer<ServerToClientEvents>,
  payload: unknown,
  context: WebSocketContext
) => void | Promise<void>

/**
 * Keeps one {@linkcode WaspSocketPeer} per connection, so that the object your
 * hooks get is the same one for as long as the connection lives.
 */
function makePeerCache<ServerToClientEvents extends EventsMap>(): (
  rawPeer: WebSocketPeer
) => WaspSocketPeer<ServerToClientEvents> {
  const peers = new WeakMap<
    WebSocketPeer,
    WaspSocketPeer<ServerToClientEvents>
  >()
  return (rawPeer) => {
    let peer = peers.get(rawPeer)
    if (peer === undefined) {
      peer = new WaspSocketPeer<ServerToClientEvents>(rawPeer)
      peers.set(rawPeer, peer)
    }
    return peer
  }
}

/**
 * Runs one of your hooks. A hook that throws must not take the whole websocket
 * (or the server) down with it, so we only report it.
 */
async function runHook(
  name: string,
  hook: () => void | Promise<void>
): Promise<void> {
  try {
    await hook()
  } catch (error) {
    console.error(`Your websocket's ${name} handler threw:`, error)
  }
}

function sendToPeers(
  peers: Iterable<WebSocketPeer>,
  event: PropertyKey,
  payload: unknown
): void {
  const message = encodeEnvelope(event, payload)
  for (const peer of peers) {
    peer.send(message)
  }
}

function encodeEnvelope(event: PropertyKey, payload: unknown): string {
  return JSON.stringify({ event: String(event), payload } satisfies Envelope)
}

function decodeEnvelope(message: WebSocketMessage): Envelope | null {
  let parsed: unknown
  try {
    parsed = JSON.parse(message.text())
  } catch {
    return null
  }

  if (
    typeof parsed !== 'object' ||
    parsed === null ||
    typeof (parsed as Envelope).event !== 'string'
  ) {
    return null
  }

  return parsed as Envelope
}

function getOfferedSubProtocols(request: Request): string[] {
  return (request.headers.get('sec-websocket-protocol') ?? '')
    .split(',')
    .map((subProtocol) => subProtocol.trim())
    .filter(Boolean)
}

{=# isAuthEnabled =}
async function getUserFromUpgradeRequest(
  request: Request
): Promise<AuthUser | null> {
  const sessionId = getSessionIdFromUpgradeRequest(request)
  if (sessionId === null) {
    return null
  }

  try {
    const sessionAndUser = await getSessionAndUserFromSessionId(sessionId)
    return sessionAndUser ? makeAuthUserIfPossible(sessionAndUser.user) : null
  } catch {
    // A connection whose session we can't read is an anonymous one, the same
    // way a request with an unusable token is.
    return null
  }
}

function getSessionIdFromUpgradeRequest(request: Request): string | null {
  const sessionIdFromSubProtocol = getOfferedSubProtocols(request)
    .find((subProtocol) => subProtocol.startsWith(authSubProtocolPrefix))
    ?.slice(authSubProtocolPrefix.length)
  if (sessionIdFromSubProtocol) {
    return sessionIdFromSubProtocol
  }

  // The escape hatch for clients that can't offer a subprotocol. It puts the
  // session ID in the URL, where proxies and access logs can see it.
  return new URL(request.url).searchParams.get(sessionIdQueryParamName)
}
{=/ isAuthEnabled =}

// PRIVATE API (SDK)
export type ClientToServerEvents = RegisteredEvents[0]
// PRIVATE API (SDK)
export type ServerToClientEvents = RegisteredEvents[1]

type RegisteredWebSocketDefinition = FromRegister<
  'webSocketFn',
  WebSocketDefinition
>
type RegisteredEvents =
  RegisteredWebSocketDefinition extends WebSocketDefinition<
    infer ClientToServerEvents,
    infer ServerToClientEvents
  >
    ? [ClientToServerEvents, ServerToClientEvents]
    : [DefaultEventsMap, DefaultEventsMap]
