import { useContext, useEffect } from 'react'
import { WebSocketContext, WebSocketContextValue } from './WebSocketProvider'
import type {
  ClientToServerEvents,
  ServerToClientEvents,
} from 'wasp/server/webSocket'

// PUBLIC API
export type ServerToClientPayload<Event extends keyof ServerToClientEvents> =
  Parameters<ServerToClientEvents[Event]>[0]
// PUBLIC API
export type ClientToServerPayload<Event extends keyof ClientToServerEvents> =
  Parameters<ClientToServerEvents[Event]>[0]

// PUBLIC API
export function useSocket(): WebSocketContextValue {
  return useContext(WebSocketContext)
}

// PUBLIC API
export function useSocketListener<Event extends keyof ServerToClientEvents>(
  event: Event,
  handler: ServerToClientEvents[Event]
): void {
  const { socket } = useContext(WebSocketContext)
  useEffect(() => {
    // Casting to `keyof ServerToClientEvents` is necessary because TypeScript
    // reports the handler function as incompatible with the event type.
    // See https://github.com/wasp-lang/wasp/pull/1203#discussion_r1232068898

    // We are wrapping it in `Extract<...>` due to Typescript infering string | number
    // in the case of default events being used.
    type AllowedEvents = Extract<keyof ServerToClientEvents, string>;
    socket.on(event as AllowedEvents, handler)
    return () => {
      socket.off(event as AllowedEvents, handler)
    }
  }, [event, handler])
}
