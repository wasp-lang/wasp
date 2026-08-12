{{={= =}=}}
import { apiEventsEmitter } from '../../api/events.js'
import { getSessionId } from '../../api/index.js'
import type {
  ClientToServerEvents,
  EventArgs,
  EventPayload,
  ServerToClientEvents,
} from '../../server/webSocket/index.js'
import { config } from '../index.js'

/** Where your app's server listens for websocket connections. */
const webSocketPath = '{= webSocketPath =}'

const shouldConnectOnStartup = {= autoConnect =}

/** Both sides always offer this one, and the server echoes it back. */
const waspSubProtocol = 'wasp'

/**
 * The subprotocol carrying the session ID. Browsers can't set headers on a
 * `WebSocket`, and the subprotocol list is the only part of the handshake that
 * reaches the server as a header instead of as part of the URL.
 */
const authSubProtocolPrefix = 'wasp-auth.'

const initialReconnectDelayMs = 500
const maxReconnectDelayMs = 10_000

type Envelope = {
  event: string
  payload?: unknown
}

type Listener = (payload: any) => void

/** Your app's websocket connection. */
// PRIVATE API (SDK)
export type WaspSocket = {
  /** Whether the connection is open right now. */
  readonly isConnected: boolean

  /** Opens the connection, and keeps reopening it whenever it drops. */
  connect(): void

  /** Closes the connection, and stops reopening it. */
  disconnect(): void

  /** Sends an event to the server. Events sent before the connection is open are sent once it is. */
  emit<Event extends keyof ClientToServerEvents>(
    event: Event,
    ...args: EventArgs<ClientToServerEvents, Event>
  ): void

  /** Runs the handler whenever the server sends the event. */
  on<Event extends keyof ServerToClientEvents>(
    event: Event,
    handler: (payload: EventPayload<ServerToClientEvents, Event>) => void
  ): void

  /** Stops running a handler registered with {@linkcode on}. */
  off<Event extends keyof ServerToClientEvents>(
    event: Event,
    handler: (payload: EventPayload<ServerToClientEvents, Event>) => void
  ): void

  /** Runs the listener whenever the connection opens or closes. Returns a function undoing it. */
  onConnectionChange(listener: (isConnected: boolean) => void): () => void

  /** Reopens the connection, so that it identifies as whoever is logged in now. */
  reauthenticate(): void
}

function createWaspSocket(): WaspSocket {
  const listeners = new Map<string, Set<Listener>>()
  const connectionListeners = new Set<(isConnected: boolean) => void>()
  /** Events emitted before the connection was open, sent as soon as it is. */
  const outbox: string[] = []

  let webSocket: WebSocket | null = null
  /**
   * Which connection attempt we are on. Every handler below checks it, because
   * closing a websocket is asynchronous: a connection we have already replaced
   * still reports its events, and its `onclose` would otherwise open a second,
   * duplicate connection.
   */
  let generation = 0
  let reconnectAttempt = 0
  let reconnectTimeout: ReturnType<typeof setTimeout> | undefined
  let shouldBeConnected = false

  function open(): void {
    clearTimeout(reconnectTimeout)

    const thisGeneration = ++generation
    const thisWebSocket = new WebSocket(getWebSocketUrl(), getSubProtocols())
    webSocket = thisWebSocket

    thisWebSocket.onopen = () => {
      if (thisGeneration !== generation) {
        return thisWebSocket.close()
      }
      reconnectAttempt = 0
      for (const message of outbox.splice(0)) {
        thisWebSocket.send(message)
      }
      reportConnectionChange(true)
    }

    thisWebSocket.onmessage = (message) => {
      if (thisGeneration !== generation) {
        return
      }
      const envelope = decodeEnvelope(message.data)
      if (envelope === null) {
        return
      }
      for (const listener of listeners.get(envelope.event) ?? []) {
        listener(envelope.payload)
      }
    }

    thisWebSocket.onclose = () => {
      if (thisGeneration !== generation) {
        return
      }
      reportConnectionChange(false)
      if (shouldBeConnected) {
        reconnectTimeout = setTimeout(
          open,
          Math.min(
            initialReconnectDelayMs * 2 ** reconnectAttempt++,
            maxReconnectDelayMs
          )
        )
      }
    }

    // A websocket that fails always closes right after, so we let `onclose`
    // do the reporting and the reconnecting.
    thisWebSocket.onerror = () => thisWebSocket.close()
  }

  function reportConnectionChange(isConnected: boolean): void {
    for (const listener of connectionListeners) {
      listener(isConnected)
    }
  }

  return {
    get isConnected() {
      return isOpen(webSocket)
    },

    connect() {
      if (!canUseWebSockets() || shouldBeConnected) {
        return
      }
      shouldBeConnected = true
      reconnectAttempt = 0
      open()
    },

    disconnect() {
      shouldBeConnected = false
      clearTimeout(reconnectTimeout)
      // Orphans the handlers of the connection we are closing: it must not
      // reopen itself, and it reports its close through us instead.
      generation++
      webSocket?.close()
      webSocket = null
      reportConnectionChange(false)
    },

    emit(event, ...args) {
      const message = encodeEnvelope(event, (args as unknown[])[0])
      if (isOpen(webSocket)) {
        webSocket.send(message)
      } else {
        outbox.push(message)
      }
    },

    on(event, handler) {
      const eventListeners = listeners.get(String(event)) ?? new Set()
      eventListeners.add(handler as Listener)
      listeners.set(String(event), eventListeners)
    },

    off(event, handler) {
      listeners.get(String(event))?.delete(handler as Listener)
    },

    onConnectionChange(listener) {
      connectionListeners.add(listener)
      return () => connectionListeners.delete(listener)
    },

    reauthenticate() {
      if (!shouldBeConnected) {
        // The next connection reads the session ID again anyway.
        return
      }
      const previousWebSocket = webSocket
      reconnectAttempt = 0
      // Bumps the generation, so the connection we are replacing goes quiet.
      open()
      previousWebSocket?.close()
    },
  }
}

function getWebSocketUrl(): string {
  // An empty `apiUrl` means the app's server is the one serving this page.
  const url = new URL(webSocketPath, config.apiUrl || window.location.href)
  url.protocol = url.protocol === 'https:' ? 'wss:' : 'ws:'
  return url.toString()
}

function getSubProtocols(): string[] {
  const sessionId = getSessionId()
  return sessionId === null
    ? [waspSubProtocol]
    : [waspSubProtocol, `${authSubProtocolPrefix}${sessionId}`]
}

/**
 * Also tells TypeScript the connection is there, and reads `WebSocket.OPEN`
 * only when it is (there is no `WebSocket` while rendering on the server).
 */
function isOpen(webSocket: WebSocket | null): webSocket is WebSocket {
  return webSocket !== null && webSocket.readyState === WebSocket.OPEN
}

function encodeEnvelope(event: PropertyKey, payload: unknown): string {
  return JSON.stringify({ event: String(event), payload } satisfies Envelope)
}

function decodeEnvelope(data: unknown): Envelope | null {
  if (typeof data !== 'string') {
    return null
  }

  let parsed: unknown
  try {
    parsed = JSON.parse(data)
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

/** Websockets are a browser thing: there is none while the app is being rendered on the server. */
function canUseWebSockets(): boolean {
  return !import.meta.env.SSR && typeof WebSocket !== 'undefined'
}

// PRIVATE API (SDK)
export const socket: WaspSocket = createWaspSocket()

// Logging in or out changes who the connection belongs to, and the server only
// reads that while it is being opened.
apiEventsEmitter.on('sessionId.set', () => socket.reauthenticate())
apiEventsEmitter.on('sessionId.clear', () => socket.reauthenticate())

if (shouldConnectOnStartup) {
  socket.connect()
}
