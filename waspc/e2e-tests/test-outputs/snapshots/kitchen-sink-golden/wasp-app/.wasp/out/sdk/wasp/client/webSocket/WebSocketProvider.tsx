import { createContext, useState, useEffect, Context, ReactNode } from 'react'
import { io, Socket } from 'socket.io-client'

import { getSessionId } from 'wasp/client/api'
import { apiEventsEmitter } from 'wasp/api/events'
import { config } from 'wasp/client'

import type { ClientToServerEvents, ServerToClientEvents } from 'wasp/server/webSocket';

export type WebSocketContextValue = {
  socket: typeof socket
  isConnected: boolean
}

// PRIVATE API
// TODO: In the future, it would be nice if users could pass more
// options to `io`, likely via some `configFn`.
export const socket: Socket<ServerToClientEvents, ClientToServerEvents> = io(
  config.apiUrl,
  {
    transports: ['websocket'],
    autoConnect: true,
  }
)

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
export const WebSocketContext: Context<WebSocketContextValue> = createContext<WebSocketContextValue>({
  socket,
  isConnected: false,
});

// PRIVATE API
export function WebSocketProvider({ children }: { children: ReactNode }) {
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
