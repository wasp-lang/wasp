import { useContext, useEffect } from 'react'

import type {
  ClientToServerEvents,
  EventPayload,
  ServerToClientEvents,
} from '../../server/webSocket/index.js'
import { WebSocketContext, WebSocketContextValue } from './WebSocketProvider'

// PUBLIC API
export type ServerToClientPayload<Event extends keyof ServerToClientEvents> =
  EventPayload<ServerToClientEvents, Event>
// PUBLIC API
export type ClientToServerPayload<Event extends keyof ClientToServerEvents> =
  EventPayload<ClientToServerEvents, Event>

// PUBLIC API
export function useSocket(): WebSocketContextValue {
  return useContext(WebSocketContext)
}

// PUBLIC API
export function useSocketListener<Event extends keyof ServerToClientEvents>(
  event: Event,
  handler: (payload: ServerToClientPayload<Event>) => void
): void {
  const { socket } = useContext(WebSocketContext)
  useEffect(() => {
    socket.on(event, handler)
    return () => {
      socket.off(event, handler)
    }
  }, [event, handler])
}
