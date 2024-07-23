{{={= =}=}}
import { createContext, useState, useEffect, Context } from 'react'
import { io, Socket } from 'socket.io-client'

import { getSessionId } from 'wasp/client/api'
import { apiEventsEmitter } from 'wasp/api/events'
import { config } from 'wasp/client'

import type { ClientToServerEvents, ServerToClientEvents } from 'wasp/server/webSocket';

{=# configFn.isDefined =}
{=& configFn.importStatement =}
{=/ configFn.isDefined =}

// PUBLIC API
export type WebSocketClientConfigFn = () => IoConfig

type IoConfig = Parameters<typeof io>[1]

{=# configFn.isDefined =}
const ioConfig = {= configFn.importIdentifier =}()
{=/ configFn.isDefined =}
{=^ configFn.isDefined =}
const ioConfig = {
  autoConnect: {= autoConnect =},
  transports: ['websocket'],
} satisfies IoConfig;
{=/ configFn.isDefined =}

// PRIVATE API
export const socket: Socket<ServerToClientEvents, ClientToServerEvents> = io(config.apiUrl, ioConfig)

function refreshAuthToken() {
  // NOTE: When we figure out how `auth: true` works for Operations, we should
  // mirror that behavior here for WebSockets. Ref: https://github.com/wasp-lang/wasp/issues/1133
  socket.auth = {
    sessionId: getSessionId()
  }

  if (socket.connected) {
    socket.disconnect()
    socket.connect()
  }
}

refreshAuthToken()
apiEventsEmitter.on('sessionId.set', refreshAuthToken)
apiEventsEmitter.on('sessionId.clear', refreshAuthToken)

// PRIVATE API
export type WebSocketContextValue = {
  socket: typeof socket
  isConnected: boolean
}

// PRIVATE API
export const WebSocketContext: Context<WebSocketContextValue> = createContext<WebSocketContextValue>({
  socket,
  isConnected: false,
});

// PRIVATE API
export function WebSocketProvider({ children }: { children: JSX.Element }) {
  const [isConnected, setIsConnected] = useState(socket.connected)

  useEffect(() => {
    function onConnect() {
      setIsConnected(true)
    }

    function onDisconnect() {
      setIsConnected(false)
    }

    socket.on('connect', onConnect)
    socket.on('disconnect', onDisconnect)

    return () => {
      socket.off('connect', onConnect)
      socket.off('disconnect', onDisconnect)
    }
  }, [])

  return (
    <WebSocketContext.Provider value={{ socket, isConnected }}>
      {children}
    </WebSocketContext.Provider>
  );
}
