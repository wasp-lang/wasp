{{={= =}=}}
import { createContext, useState, useEffect } from 'react'
import { io, Socket } from 'socket.io-client'

import { getAuthToken } from '../api'
import { apiEventsEmitter } from '../api/events'
import config from '../config'

import type { ClientToServerEvents, ServerToClientEvents } from '../webSocket';

// TODO: In the future, it would be nice if users could pass more
// options to `io`, likely via some `configFn`.
export const socket: Socket<ServerToClientEvents, ClientToServerEvents> = io(config.apiUrl, { autoConnect: {= autoConnect =} })

function refreshAuthToken() {  
  // NOTE: When we figure out how `auth: true` works for Operations, we should
  // mirror that behavior here for WebSockets. Ref: https://github.com/wasp-lang/wasp/issues/1133
  socket.auth = {
    token: getAuthToken()
  }

  if (socket.connected) {
    socket.disconnect()
    socket.connect()
  }
}

refreshAuthToken()
apiEventsEmitter.on('authToken.set', refreshAuthToken)
apiEventsEmitter.on('authToken.clear', refreshAuthToken)

export const WebSocketContext = createContext({
  socket,
  isConnected: false,
});

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
