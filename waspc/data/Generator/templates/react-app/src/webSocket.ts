import { useContext, useEffect } from 'react'
import { WebSocketContext } from './webSocket/WebSocketProvider';

import type {
  ClientToServerEvents as _ClientToServerEvents,
  ServerToClientEvents as _ServerToClientEvents
} from '../../server/src/webSocket'

export type ClientToServerEvents = _ClientToServerEvents
export type ServerToClientEvents = _ServerToClientEvents

export function useSocket() {
  return useContext(WebSocketContext);
}

export function useSocketListener<Event extends keyof ServerToClientEvents>(
  event: Event,
  handler: ServerToClientEvents[Event]
) {
  const { socket } = useContext(WebSocketContext);
  useEffect(() => {
    socket.on(event, handler)
    return () => {
      socket.off(event, handler)
    }
  }, [])
}
