import { createContext, useEffect, useState, Context, ReactNode } from 'react'

import { socket, type WaspSocket } from './socket.js'

// PRIVATE API (SDK)
export type WebSocketContextValue = {
  socket: WaspSocket
  isConnected: boolean
}

// PRIVATE API (SDK)
export const WebSocketContext: Context<WebSocketContextValue> =
  createContext<WebSocketContextValue>({
    socket,
    // Our hooks need to be SSR-safe, and websockets don't work on the server,
    // so we start with `false` and let the browser correct it.
    isConnected: false,
  })

// PRIVATE API (SDK)
export function WebSocketProvider({ children }: { children: ReactNode }) {
  const [isConnected, setIsConnected] = useState(false)

  useEffect(() => {
    setIsConnected(socket.isConnected)
    return socket.onConnectionChange(setIsConnected)
  }, [])

  return (
    <WebSocketContext.Provider value={{ socket, isConnected }}>
      {children}
    </WebSocketContext.Provider>
  )
}
