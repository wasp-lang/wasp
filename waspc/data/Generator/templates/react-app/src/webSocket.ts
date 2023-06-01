{{={= =}=}}

import { useState, useEffect } from 'react'
import { io, Socket } from 'socket.io-client'
import { DefaultEventsMap, EventsMap } from '@socket.io/component-emitter'

import { getAuthToken, events as apiEvents, EventType } from './api'
import config from './config'

// TODO: In the future, it would be nice if users could pass more
// options to `io`, likely via some `configFn`.
export const socket = io(config.apiUrl, { autoConnect: {= autoConnect =} })

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
apiEvents.on(EventType.SET_AUTH_TOKEN, refreshAuthToken)
apiEvents.on(EventType.CLEAR_AUTH_TOKEN, refreshAuthToken)

export function useSocket<
  ServerToClientEvents extends EventsMap = DefaultEventsMap,
  ClientToServerEvents extends EventsMap = DefaultEventsMap
>(): {
  socket: Socket<ServerToClientEvents, ClientToServerEvents>;
  isConnected: boolean;
  registerHandler: typeof registerHandler;
} {
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

  function registerHandler<Event extends keyof ServerToClientEvents>
    (event: Extract<Event, string>, cb: ServerToClientEvents[Event]) {
    useEffect(() => {
      socket.on(event, cb)
      return () => {
        socket.off(event, cb)
      }
    }, [])
  }  

  return { socket, isConnected, registerHandler }
}
