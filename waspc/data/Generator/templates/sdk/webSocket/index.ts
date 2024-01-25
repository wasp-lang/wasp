import { useContext, useEffect } from 'react'
import { WebSocketContext } from './WebSocketProvider'
import type {
  ClientToServerEvents,
  ServerToClientEvents,
} from 'wasp/server/webSocket'

export type ServerToClientPayload<Event extends keyof ServerToClientEvents> =
  Parameters<ServerToClientEvents[Event]>[0]
export type ClientToServerPayload<Event extends keyof ClientToServerEvents> =
  Parameters<ClientToServerEvents[Event]>[0]

export function useSocket() {
  return useContext(WebSocketContext)
}

export function useSocketListener<Event extends keyof ServerToClientEvents>(
  event: Event,
  handler: ServerToClientEvents[Event]
) {
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
