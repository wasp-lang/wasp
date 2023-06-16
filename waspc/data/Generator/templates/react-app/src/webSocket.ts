import { useContext, useEffect } from 'react'
import { WebSocketContext } from './webSocket/WebSocketProvider';

export type {
  ClientToServerEvents,
  ServerToClientEvents,
} from '../../server/src/webSocket'

import type { ServerToClientEvents } from '../../server/src/webSocket'

export function useSocket() {
  return useContext(WebSocketContext);
}

export function useSocketListener<Event extends keyof ServerToClientEvents>(
  event: Event,
  handler: ServerToClientEvents[Event],
) {
  const { socket } = useContext(WebSocketContext);
  useEffect(() => {
    /*
    TODO(miho): This is a hack for type error we are getting
    from socket.on when passing in "handler" directly:
    
    Argument of type 'ServerToClientEvents[Event]' is not assignable to parameter of type
      'FallbackToUntypedListener<
        Event extends "disconnect" | "connect" | "connect_error"
          ? SocketReservedEvents[Event]
          : Event extends "chatMessage"
            ? ServerToClientEvents[Event]
            : never
      >'.

    It looks like it should be assignable, but it's not.
    */
    const handlerInstance: any = (event: any) => {
      handler(event);
    };
    socket.on(event, handlerInstance);
    return () => {
      socket.off(event, handlerInstance);
    };
  }, [event, handler]);
}
